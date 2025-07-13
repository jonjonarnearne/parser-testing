module Parser where

import System.IO (Handle)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

printIt :: String -> IO ()
printIt = putStrLn

data ParseState = ParseState {
       string :: L.ByteString
     , offset :: Int64
     } deriving (Show)

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset s i = s { offset = i }

-- Take state, return some value and state
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

-- Take state, return error or value and state
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

(==>) :: Parse a -> (a -> Parse b) -> Parse b
a ==> b = Parse chainedParser
  where chainedParser initState =
          case runParse a initState of
            Left err                 -> Left err
            Right (result, newState) ->
              runParse (b result) newState


-- Identity parser the lambda is the actual parse function
identity :: a -> Parse a
identity a = Parse $ \s -> Right (a, s)

initParser :: L.ByteString -> Parse ()
initParser bytes = Parse $ \_ -> Right ((), ParseState bytes 0)

parseHeader :: L.ByteString -> Parse Bool
parseHeader needle =
    getState ==> \initState ->
      case L8.isPrefixOf needle (string initState) of
        False            -> bail "Prefix not found"
        True             ->
           putState newState ==> \_ ->
           identity True
         where newState = initState { string = newString, offset = newOffset }
               newOffset = offset initState + L8.length needle
               newString = L8.dropWhile isSpace (L8.drop (L8.length needle) $ string initState)

parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
      case L.uncons (string initState) of
        Nothing           -> bail "No more input"
        Just (byte, rest) ->
          putState newState ==> \_ ->
          identity byte
         where newState = initState { string = rest, offset = newOffset }
               newOffset = offset initState + 1

parseReadInt :: Parse Int
parseReadInt =
    getState ==> \initState ->
      case L8.readInt (string initState) of
        Nothing -> bail "No more input"
        Just (num, rest)
          | num <= 0 -> bail $ "Expected natural, got: " ++ show num
          | otherwise -> putState newState ==> \_ ->
                identity num -- $ fromIntegral num
          where newState = initState { string = rest, offset = newOffset }
                newOffset = offset initState + (L8.length (string initState) - L8.length rest)

parseSkipSpace :: Parse ()
parseSkipSpace =
    getState ==> \initState ->
      let newString = L8.dropWhile isSpace $ string initState
          newOffset = L8.length (string initState) - L8.length newString + offset initState
       in putState $ initState { string = newString, offset = newOffset }

{-
getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes l s = let count         = fromIntegral l
                   both@(pfx, _) = L.splitAt count s
                in if L.length pfx < count
                  then Nothing
                  else Just both
-}
parseReadBytes :: Int -> Parse L.ByteString
parseReadBytes l =
    getState ==> \initState ->
     let count       = fromIntegral l
         (pfx, rest) = L.splitAt count $ string initState
         newOffset = L8.length (string initState) - L8.length rest + offset initState
         newState    = initState { string = rest, offset = newOffset }
      in if L.length pfx < count
        then bail $ "Could not parse " ++ show count ++ " bytes"
        else putState newState ==> \_ -> identity pfx

parseMatchP5 :: Parse L.ByteString
parseMatchP5 =
  parseHeader (L8.pack "P5")      ==>
  const parseReadInt              ==> \width -> -- Width
  parseSkipSpace                  ==>
  const parseReadInt              ==> \height -> -- Height
  parseSkipSpace                  ==>
  const parseReadInt              ==> \_ ->      -- Max gray
  parseReadBytes (width * height)

getState :: Parse ParseState
getState = Parse $ \s -> Right (s, s)

putState :: ParseState -> Parse ()
putState s = Parse $ \_ -> Right ((), s)

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
            "byte offset " ++ show (offset s) ++ ": " ++ err


parse :: Parse a -> L.ByteString -> Either String a
parse parser bytes
    = case runParse parser (ParseState bytes 0) of
        Left err          -> Left err
        Right (result, _) -> Right result

data Graymap = Graymap {
       bWidth  :: Int
     , bHeight :: Int
     , bMax    :: Int
     , bData  :: L.ByteString
     } deriving (Eq)
instance Show Graymap where
  show (Graymap w h m _) = "Graymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

openFile :: IO Handle -> Graymap
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


parseP5 :: L.ByteString -> Maybe (Graymap, L.ByteString)
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
  \(bytes,s6) -> Just (Graymap width height maxB bytes, s6)

