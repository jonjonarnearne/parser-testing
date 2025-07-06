module Main where

import System.IO
import System.Exit (exitFailure)
--import System.Environment (getArgs, getProgName)
import qualified Data.ByteString.Lazy as L

import Parser

main :: IO ()
main = do
    binary <- openBinaryFile "baboon.pgm" ReadMode
    bD <- L.hGetContents binary
    gM <- case parseP5 bD of
      Nothing -> hPutStrLn stderr "parse failed" >> exitFailure
      Just (m, _) -> return m
    printIt $ "Hello world:" ++ show gM
