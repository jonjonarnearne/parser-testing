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
    gM <- case parse parseGraymap bD of
      Left err -> hPutStrLn stderr ("parse failed: " ++ err) >> exitFailure
      Right m  -> return m
    printIt $ "Graymap: " ++ show gM ++ " - data length: " ++ show (L.length $ bData gM)
