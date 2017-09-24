{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split
import Data.Csv
import qualified Data.Text              as T
import qualified Data.Attoparsec.Text   as A
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Control.Arrow (first)
import Control.Monad.State

import qualified Sgf.Data.Text.Table    as T


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
                        { number        :: Int
                        , reference     :: String
                        , reading1      :: String
                        , reading2      :: String
                        , origin        :: String
                        , translate     :: String
                        , description   :: String
                        , tags          :: String
                        }
  deriving (Show, Read)
defJWord :: JWord
defJWord            = JWord
                        { number        = 0
                        , reference     = ""
                        , reading1      = ""
                        , reading2      = ""
                        , origin        = ""
                        , translate     = ""
                        , description   = ""
                        , tags          = ""
                        }

instance T.FromTable JWord where
    parseTable      = T.withTableText "JWord" $ \m ->
        JWord
            <$> m T..: "Num"
            <*> (T.unpack <$> m T..: "Reference")
            <*> (T.unpack <$> m T..: "Reading1")
            <*> (T.unpack <$> m T..: "Reading2")
            <*> (T.unpack <$> m T..: "Origin")
            <*> (T.unpack <$> m T..: "Translation")
            <*> (T.unpack <$> m T..: "Description")
            <*> (T.unpack <$> m T..: "Tags")

instance Serialize JWord where
    fromString k    = flip runStateT k $ do
        [n, ref, r1, r2, on, tr, ds, ts] <- toReadM (readLine ':')
        return JWord
            { number        = read n
            , reference     = ref
            , reading1      = r1
            , reading2      = r2
            , origin        = on
            , translate     = tr
            , description   = ds
            , tags          = ts
            }

instance ToRecord JWord where
    toRecord JWord {..} = record
                            [ toField number
                            , toField reference
                            , toField reading1
                            , toField reading2
                            , toField origin
                            , toField translate
                            , toField description
                            , toField tags
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

buildMap :: (a -> Int) -> [a] -> M.Map Int [a]
buildMap toKey      = foldr (\x -> M.insertWith (++) (toKey x) [x]) M.empty

writeMap :: ToRecord a => FilePath -> M.Map Int [a] -> IO ()
writeMap f          = L.writeFile f . encode . concat . M.elems

checkMap :: M.Map Int [a] -> IO ()
checkMap m          = do
    let ds = M.filter ((> 1) . length) m
    print $ "Duplicate numbers: " ++ show (M.keys ds)
    print $ "Max number: " ++ show (fst $ M.findMax m)

main :: IO ()
main = do
    print "words:"
    ws <-   readFile "../words.txt" >>=
            return . buildMap number . concatMap fst . parseAll
    checkMap ws
    writeMap "words.csv" ws

    print "words mnn:"
    mws <-  T.decodeFile "../words-mnn.txt" >>=
            either (\e -> error $ "Can't parse JWords table " ++ e)
                   (return . buildMap number)
    checkMap mws
    writeMap "words-mnn.csv" mws

    kw <- readFile "../kana.txt"
    let ks = concatMap fst (parseAll kw) :: [JKana]
    L.writeFile "kana.csv" (encode ks)

