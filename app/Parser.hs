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

matchP5Header :: L.ByteString -> Maybe L.ByteString
matchP5Header = matchHeader $ L8.pack "P5"

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num, rest)
               | num <= 0  -> Nothing
               | otherwise -> Just (fromIntegral num, rest)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, b) = Just (a, L8.dropWhile isSpace b)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes l s = let count         = fromIntegral l
                   both@(pfx, _) = L.splitAt count s
                in if L.length pfx < count
                  then Nothing
                  else Just both

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v


parseP5 :: L.ByteString -> Maybe (ParserState, L.ByteString)
parseP5 s = matchP5Header s                            >>?
  \s1 -> skipSpace ((), s1)                            >>?
  (getNat . snd)                                       >>?
  skipSpace                                            >>?
  \(width, s2) -> getNat s2                            >>?
  skipSpace                                            >>?
  \(height,  s3) -> getNat s3                          >>?
  \(maxB,s4) -> if maxB > 255
    then Nothing
    else getBytes 1 s4                                 >>?
  \(_,s5) -> getBytes (width * height) s5              >>?
  \(bytes,s6) -> Just (ParserState width height maxB bytes, s6)

