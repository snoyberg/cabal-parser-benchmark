{-# LANGUAGE PackageImports #-}
module Main where

import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Archive.Tar as Tar
import System.Clock
import System.IO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified "Cabal" Distribution.PackageDescription as OldParser
import qualified "Cabal" Distribution.PackageDescription.Parse as OldParser

import qualified "Cabal-snoy" Distribution.PackageDescription as Parsec
import qualified "Cabal-snoy" Distribution.PackageDescription.Parse as Parsec

timed :: String -> IO () -> IO ()
timed label action = do
  start <- getTime Monotonic
  action
  end <- getTime Monotonic
  putStrLn $ label ++ ": " ++ show (toNanoSecs (diffTimeSpec end start)) ++ "ns"

test :: String -> (FilePath -> String -> IO ()) -> IO ()
test label parser = do
  dir <- getAppUserDataDirectory "stack"
  let fp = dir </> "indices" </> "Hackage" </> "01-index.tar"
  withBinaryFile fp ReadMode $ \h -> do
    lbs <- BL.hGetContents h
    let loop Tar.Done = return ()
        loop (Tar.Fail e) = error $ show e
        loop (Tar.Next e es) = goE e >> loop es

        goE e =
          case Tar.entryContent e of
            Tar.NormalFile lbs _ | takeExtension (Tar.entryPath e) == ".cabal" ->
              case TL.decodeUtf8' lbs of
                Left e' -> return () -- error $ show (Tar.entryPath e, e')
                Right text -> parser (Tar.entryPath e) $ TL.unpack $ TL.dropWhile (== '\xFEFF') text
            _ -> return ()
    timed label $ loop $ Tar.read lbs

main :: IO ()
main = do
  test "OldParser" $ \fp str ->
    case OldParser.parsePackageDescription str of
      OldParser.ParseFailed err -> error $ show (fp, err)
      OldParser.ParseOk _ _ -> return ()
  test "Parsec" $ \fp str ->
    case OldParser.parsePackageDescription str of
      OldParser.ParseFailed err -> error $ show (fp, err)
      OldParser.ParseOk _ _ -> return ()
