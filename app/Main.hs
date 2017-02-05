{-# LANGUAGE RecordWildCards     #-}

module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Csv
import Data.Function
import qualified Data.ByteString.Lazy as L
import Control.DeepSeq
import Control.Arrow
import Lib


-- | One step in recursively string parsing: if there is no more input left,
-- finish with current result. Otherwise, try to read on more value. Here
-- recursive call returns to caller, so i may add current step's result then
-- to preserve input element order.
step :: Serialize a => ([a] -> ReadS [a])    -- ^ Recurse call.
        -> [a]                          -- ^ Current result.
        -> ReadS [a]
step rec zs []      = [(zs, [])]
step rec zs s       = case fromString s of
    [] -> rec zs []
    xs -> concatMap (\(x, s) -> fmap (first (x :)) (rec zs s)) xs

parseAll :: Serialize a => ReadS [a]
parseAll            = fix step []

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

instance Serialize JWord where
    fromString      = go . span (/= '\n')
      where
        go :: (String, String) -> [(JWord, String)]
        go ([],      [])    = []
        go ([], (_ : r))    = fromString r    -- Empty string.
        go (l, r)   =
            let xs = splitOn " - " l
                ys = map (dropWhileEnd isSpace . dropWhile isSpace) xs
                [r1, r2, or, tr, ref] = ys
            in  [( JWord { reading1 = r1
                         , reading2 = r2
                         , origin   = or
                         , translate = tr
                         , reference = ref
                         }
                 , drop 1 r
                 )]

instance ToRecord JWord where
    toRecord (JWord {..})   = record [ toField reading1
                                     , toField reading2
                                     , toField origin
                                     , toField translate
                                     , toField reference
                                     ]

t :: String
t   = unlines [""
    , "ざっし      - ザッシ        -           - журнал                    - С3-1"
    , "ひと        - ヒト          - 人        - человек                   - С3-2"
    , "しゃいん    - シャイン      - 社員      - сотрудник компании        - "
    , ""
    ]

main :: IO ()
main = do
    c <- readFile "../words.txt"
    return (rnf c)
    --print (fmap (first (take 1)) $ (parseAll c :: [([JWord], String)]))
    let [(ws, _)] = parseAll c :: [([JWord], String)]
    L.writeFile "1.csv" (encode ws)

