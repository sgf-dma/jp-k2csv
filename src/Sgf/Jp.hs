
module Sgf.Jp
    ( buildMap
    , writeMap
    , checkMap
    , possibleForeign
    , checkRefs
    , toWords
    , inConjTags
    )
  where

import           Data.Char (isAscii)
import qualified Data.Map               as M
import qualified Data.ByteString.Lazy   as BL
import           Data.Csv
import           Control.Monad

import           Turtle.Shell
import           Turtle.Line
import qualified Data.String as S

import Sgf.Jp.Types
import Sgf.JPWords.Checks
import Sgf.Data.Text.Parse (toWords)


buildMap :: (a -> Int) -> [a] -> M.Map Int [a]
buildMap toKey = foldr (\x -> M.insertWith (++) (toKey x) [x]) M.empty

writeMap :: ToRecord a => FilePath -> M.Map Int [a] -> IO ()
writeMap f = BL.writeFile f . encode . concat . M.elems

checkMap :: M.Map Int [a] -> IO ()
checkMap m = do
    let ds = M.filter ((> 1) . length) m
    print $ "Duplicate numbers: " ++ show (M.keys ds)
    print $ "Max number: " ++ show (fst $ M.findMax m)

-- | Possible foreign words. Though, filtering may be greatly improved..
possibleForeign :: M.Map Int [JWord] -> M.Map Int [JWord]
possibleForeign = M.map (filter (all isAscii . origin))

checkRefs :: M.Map Int [JWord] -> IO ()
checkRefs = view . bothKanjiRefAndRel . onlyRefs
  where
    onlyRefs :: M.Map Int [JWord] -> Shell Line
    onlyRefs =
        select . textToLines . S.fromString . reference <=< select <=< select

