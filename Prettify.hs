module Prettify 
  ( Doc
  , (<>), (</>), hcat, fsep, fold, group
  , punctuate, series, enclose, fill, nest
  , empty, line, softline
  , string, double, text, char, oneChar
  , compact, pretty, nestedPretty
  ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

data Doc = DEmpty 
         | DText String
         | DChar Char
         | DLine
         | DConcat Doc Doc
         | DUnion Doc Doc
           deriving (Show, Eq) 

(<>) :: Doc -> Doc -> Doc
DEmpty <>      b = b
a      <> DEmpty = a
a      <>      b = a `DConcat` b 

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold = flip foldr empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

group :: Doc -> Doc
group x = flatten x `DUnion` x


flatten :: Doc -> Doc
flatten (x `DConcat` y) = flatten x `DConcat` flatten y
flatten DLine           = char ' '
flatten (x `DUnion`  _) = flatten x
flatten other           = other


punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ d@[_] = d
punctuate p (d:ds) = (d <> p) : punctuate p ds

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series left right converter = enclose left right   . fsep 
                            . punctuate (char ',') . map converter

enclose :: Char -> Char -> Doc -> Doc
enclose left right doc = char left <> doc <> char right

fill :: Int -> Doc -> Doc
fill n d = d `DConcat` (text $ replicate n ' ')

nest :: Doc -> Doc
nest d = nest' 0 [] [d]
  where nest' :: Int -> [Int] -> [Doc] -> Doc 
        nest'   _             _     [] = DEmpty
        nest' col            []     ds = nest' col [col] ds 
        nest' col indents@(n:_) (d:ds) = 
          let padding = text $ replicate n ' '
          in case d of 
            DEmpty        -> nest' col indents ds
            s@(DText t)   -> s <> nest' (col + length t) indents ds
            c@(DChar c')  -> c <> nest' (col + 1) (updateStack c' col indents) ds 
            DLine         -> DLine <> padding <> nest' col indents ds
            a `DConcat` b -> nest' col indents (a:b:ds)
            _ `DUnion`  b -> nest' col indents (b:ds)
          where updateStack :: Char -> Int -> [Int] -> [Int]
                updateStack c col indents@(_:ns)
                  | c `elem` "[{(" = (col:indents)
                  | c `elem` ")}]" = ns
                  | otherwise      = indents


empty, line :: Doc
empty = DEmpty
line  = DLine

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

double :: Double -> Doc
double = text . show 

text :: String -> Doc
text "" = DEmpty
text s  = DText s  

char :: Char -> Doc 
char = DChar

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just      r            -> text r 
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c == '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

hexEscape :: Char -> Doc
hexEscape c 
  | d < 0x10000 = smallHex d
  | otherwise   = astral (d - 0x10000)  
  where d = ord c
        smallHex n = let h = showHex n "" 
                     in text "\\u" <> text (replicate (4 - length h) '0') 
                                   <> text h
        astral   n = let a = (n `shiftR` 10) .&. 0x3ff
                         b = n .&. 0x3ff
                     in smallHex (a + 0xd800) <> smallHex (b + 0xdc00)


--67.6 s, 35211248104 bytes
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
            case d of
              DEmpty        -> transform ds
              DChar c       -> c : transform ds
              DText s       -> s ++ transform ds
              DLine         -> '\n' : transform ds
              a `DConcat` b -> transform (a:b:ds)
              _ `DUnion` b  -> transform (b:ds)

--68.8 s, 36721005632 bytes
compact' :: Doc -> String
compact' DEmpty          = ""
compact' (DChar c)       = [c]
compact' (DText t)       = t
compact' DLine           = "\n"
compact' (a `DConcat` b) = transform [a, b]
  where transform = concatMap compact'
compact' (a `DUnion`  b) = compact' b

pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            DEmpty        -> best col ds
            DChar c       -> c :  best (col + 1) ds
            DText s       -> s ++ best (col + length s) ds
            DLine         -> '\n' : best 0 ds
            a `DConcat` b -> best col (a:b:ds)
            a `DUnion` b  -> nicest col (best col (a:ds))
                                        (best col (b:ds))
        best _ _ = ""
        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
          where least = min width col
        w `fits` _ | w < 0 = False  
        w `fits` ""        = True
        w `fits` ('\n':_)  = True
        w `fits` (c:cs)    = (w - 1) `fits` cs

nestedPretty :: Int -> Doc -> String
nestedPretty n = (pretty n) . nest 
