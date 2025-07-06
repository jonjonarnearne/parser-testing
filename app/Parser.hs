module Parser where

import System.IO (Handle)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

printIt :: String -> IO ()
printIt = putStrLn

data ParserState = ParserState {
       bWidth  :: Int
     , bHeight :: Int
     , bMax    :: Int
     , bData  :: L.ByteString
     } deriving (Eq)
instance Show ParserState where
  show (ParserState w h m _) = "ParserState " ++ show w ++ "x" ++ show h ++ " " ++ show m

openFile :: IO Handle -> ParserState
openFile = undefined

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader pfx str
  | pfx `L8.isPrefixOf` str = Just $ L8.dropWhile isSpace (L.drop (L.length pfx) str)
  | otherwise               = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num, rest)
               | num <= 0  -> Nothing
               | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes l s = let count         = fromIntegral l
                   both@(pfx, _) = L.splitAt count s
                in if L.length pfx < count
                  then Nothing
                  else Just both

parseP5 :: L.ByteString -> Maybe (ParserState, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxG, s4)
                  | maxG > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (ParserState width height maxG bitmap, s6)

