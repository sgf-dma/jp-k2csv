{-# LANGUAGE RecordWildCards     #-}

module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split
import Data.Csv
import qualified Data.ByteString.Lazy as L
import Control.Arrow (first)
import Control.Monad.State


-- | One step in recursively string parsing: if there is no more input left,
-- finish with current result. Otherwise, try to read on more value. Here
-- recursive call returns to caller, so i may add current step's result then
-- to preserve input element order.
step :: Serialize a => ([a] -> ReadS [a])   -- ^ Recurse call.
        -> [a]                              -- ^ Current result.
        -> ReadS [a]
step _ zs []        = [(zs, [])]
step rec zs s       = case fromString s of
    [] -> rec zs []
    xs -> concatMap (\(x, r) -> fmap (first (x :)) (rec zs r)) xs

parseAll :: Serialize a => ReadS [a]
parseAll            = fix step []

-- | Read one line and split by \" s \", where s is some character.
readLine :: Char -> ReadS [String]
readLine s          = go . span (/= '\n')
      where
        sep :: String
        sep         = [' ', s, ' ']
        go :: (String, String) -> [([String], String)]
        go ([],    [])  = []
        go ([], _ : r)  = readLine s r  -- Empty string.
        go (l , r)      =
            let xs = splitOn sep l
                ys = map (dropWhileEnd isSpace . dropWhile isSpace) xs
            in  [(ys, drop 1 r)]

type ReadM a        = StateT String [] a
toReadM :: ReadS a -> ReadM a
toReadM             = StateT

class Serialize a where
    fromString :: ReadS a
    toString :: a -> ShowS

data JWord          = JWord
                        { reading1  :: String
                        , reading2  :: String
                        , origin    :: String
                        , translate :: String
                        , reference :: String
                        }
  deriving (Show, Read)
defJWord :: JWord
defJWord            = JWord
                        { reading1  = ""
                        , reading2  = ""
                        , origin    = ""
                        , translate = ""
                        , reference = ""
                        }

instance Serialize JWord where
    fromString k    = flip runStateT k $ do
        [r1, r2, on, tr, ref] <- toReadM (readLine '-')
        return JWord
            { reading1  = r1
            , reading2  = r2
            , origin    = on
            , translate = tr
            , reference = ref
            }

instance ToRecord JWord where
    toRecord JWord {..} = record
                            [ toField reading1
                            , toField reading2
                            , toField origin
                            , toField translate
                            , toField reference
                            ]

data JKana          = JKana
                        { hiragana  :: String
                        , katakana  :: String
                        , syllable  :: String
                        , desc      :: String
                        }
  deriving (Show, Read)
defJKana :: JKana
defJKana            = JKana
                        { hiragana  = ""
                        , katakana  = ""
                        , syllable  = ""
                        , desc      = ""
                        }


instance Serialize JKana where
    fromString k    = flip runStateT k $ do
        [hn, kn, sy, dc] <- toReadM (readLine ':')
        return JKana
            { hiragana  = hn
            , katakana  = kn
            , syllable  = sy
            , desc      = dc
            }

instance ToRecord JKana where
    toRecord JKana {..} = record
                            [ toField hiragana
                            , toField katakana
                            , toField syllable
                            , toField desc
                            ]


main :: IO ()
main = do
    cw <- readFile "../words.txt"
    let ws = concatMap fst (parseAll cw) :: [JWord]
    L.writeFile "words.csv" (encode ws)
    kw <- readFile "../kana.txt"
    let ks = concatMap fst (parseAll kw) :: [JKana]
    L.writeFile "kana.csv" (encode ks)

