{-# LANGUAGE OverloadedStrings      #-}

module Main where

import Data.Csv
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map               as M

import qualified Sgf.Data.Text.Table    as T
import Sgf.Jp
import Sgf.Jp.Types
import Sgf.Data.Text.OldTable


main :: IO ()
main = do
    m <- T.decodeFileL "words-mnn.txt" >>= either
        (\e -> error $ "Can't parse JWords table " ++ e)
        (return . buildMap number)
    checkMap m
    --checkRefs m
    writeMap "foreign.csv" (possibleForeign m)
    writeMap "words.csv"   m
    writeMap "words-ndsc.csv"   (M.map (map JWordN) m)

    mconj <- T.decodeFileL "conjugations.txt" >>= either
        (\e -> error $ "Can't parse JConj table " ++ e)
        (return . buildMap conjNumber)
    checkMap mconj
    writeMap "conj.csv" mconj
    writeMap "conj-full.csv" (M.map (map JConjF) mconj)

    mjoin <- T.decodeFileL "join.txt" >>= either
        (\e -> error $ "Can't parse Join table " ++ e)
        (return . (`asTypeOf` [defJJoin]))
    BL.writeFile "join.csv" (encode mjoin)
    print $ "Join elements: " ++ show (length mjoin)

    kw <- readFile "kana.txt"
    let ks = concatMap fst (parseAll kw) :: [JKana]
    BL.writeFile "kana.csv" (encode ks)

