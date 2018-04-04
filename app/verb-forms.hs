{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module Main where

import           Data.Maybe
import qualified Data.List              as L
import           Data.List.Extra (snoc)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as BS
import qualified Data.Attoparsec.Text   as A
import           Control.Applicative
import           Control.Monad
import           System.Random.Shuffle

import qualified Sgf.Data.Text.Table    as T

import Data.Function

import Sgf.Jp
import Sgf.Jp.Types
import Sgf.Data.Text.Table.Parse

-- Take input untill separator parser succeeds. Predicate is used to identify
-- character at which to try to match separator parser. Result of separator
-- parser is returned as separate value: i.e. text matched to separator is
-- effectively dropped from result, but return value allows to pass some
-- information (like which separator matches) from separator parser to the
-- caller. Chunks not ending on separator are _not_ included.
takeTillSep :: (Char -> Bool) -> A.Parser a -> A.Parser (T.Text, a)
takeTillSep p sepP =
    (,)
        <$> (   fmap T.concat
            .   many
            $   T.append
            <$> whenNotP sepP (T.singleton <$> A.anyChar)
            <*> A.takeWhile p
            )
        <*> sepP

-- Take input till string parser succeeds. Predicate is used to identify
-- character at which to try to match string parser. Matched string is
-- _included_ in result (it will be at the end of result text). Chunks not
-- ending on matched string are _not_ included.
takeTillStr :: (Char -> Bool) -> A.Parser T.Text -> A.Parser T.Text
takeTillStr p strP =
    fmap T.concat
        .   snoc
        <$> (   many
            $   T.append
            <$> whenNotP strP (T.singleton <$> A.anyChar)
            <*> A.takeWhile p
            )
        <*> strP

-- | Split using 'takeTillSep' until separator parser returns 'True'.
splitUntil :: (Char -> Bool) -> A.Parser Bool -> A.Parser [T.Text]
splitUntil p sepP = fix (step (takeTillSep p sepP)) ([], True)

-- | One 'fix' step.
step
    :: Monad m
    => m (a, Bool)
    -> (([a], Bool) -> m [a])
    -> ([a], Bool)
    -> m [a]
step mx rec (zs, b) | b         = mx >>= \(w, b') -> (w :) <$> rec (zs, b')
                    | otherwise = return zs

-- | Word end.
wordEnd :: T.Text
wordEnd = " "
-- | Word separator.
wordSep :: T.Text
wordSep = ","

-- | Safe 'head' for 'Text'.
tHeadMay :: T.Text -> Maybe Char
tHeadMay t | T.null t  = Nothing
           | otherwise = Just (T.head t)

-- | Split to words by building separator from supplied word suffix and word
-- delimiter. The last word is the one having only suffix without word
-- delimeter at the end. No further input are parsed after last word. Word
-- suffix may be /empty/.
wordsWithSuffix :: T.Text -> T.Text -> [T.Text]
wordsWithSuffix sf = either (const []) id . A.parseOnly
    (splitUntil
        (`notElem` catMaybes
            [ tHeadMay sf
            , tHeadMay wordSep
            , tHeadMay wordEnd
            ]
        )
        (   A.string (sf `T.append` wordSep)
            *>  many (A.string wordEnd)
            *>  return True
        <|> A.string sf
            *>  (A.string wordEnd <|> A.endOfInput *> pure T.empty)
            *>  return False
        )
    )

-- | Generate several words with different suffixes in place of original.
-- Empty suffix is /allowed/.
replaceSuffix :: T.Text -> [T.Text] -> T.Text -> [VForm]
replaceSuffix sf rs t
    | null ws   = []
    | otherwise = foldr (\r -> (:) . VForm . map (`T.append` r) $ ws) [] rs
    where ws = wordsWithSuffix sf t


-- Each verb may have several writings.
newtype VForm       = VForm {vforms :: [T.Text]}
  deriving (Show)

vFormToText :: VForm -> T.Text
vFormToText (VForm xs) = T.concat . L.intersperse ", " $ xs

appendConjNum :: VForm -> JConj -> VForm
appendConjNum VForm {..} = VForm . snoc vforms . T.pack . show . conjNumber

-- FIXME: dict form shouldn't work with several kanji.
genDictForms :: Bool -> JConj -> [VForm]
genDictForms isKanji = gen . dictStem >>= mapM appendConjNum
  where
    dictStem :: JConj -> T.Text
    dictStem x | not isKanji || null (dictFormK x) = T.pack (dictForm x)
               | otherwise                         = T.pack (dictFormK x)
    gen :: T.Text -> [VForm]
    gen = "" `replaceSuffix` ["前に", "ことができます"]

-- FIXME: The same problem, as with dict forms?
genMasuForms :: Bool -> JConj -> [VForm]
genMasuForms isKanji w =
    let t = masuStem w in mapM appendConjNum (VForm [t] : gen t) w
  where
    masuStem :: JConj -> T.Text
    masuStem x | not isKanji || null (masuFormK x) = T.pack (masuForm x)
               | otherwise                         = T.pack (masuFormK x)
    gen :: T.Text -> [VForm]
    gen = "ます" `replaceSuffix` ["たい", "たくない", "たかった", "たくなかった"]

genTeForms :: Bool -> JConj -> [VForm]
genTeForms isKanji = (gen <++> gen') . teStem >>= mapM appendConjNum
  where
    teStem :: JConj -> T.Text
    teStem x | not isKanji || null (teFormK x) = T.pack (teForm x)
             | otherwise                       = T.pack (teFormK x)
    gen :: T.Text -> [VForm]
    gen = "て" `replaceSuffix` ["てください", "てもいいです", "てはいけません", "ています"]
    gen' :: T.Text -> [VForm]
    gen' = "で" `replaceSuffix` ["でください", "でもいいです", "ではいけません", "でいます"]

genTaForms :: Bool -> JConj -> [VForm]
genTaForms isKanji = (gen <++> gen') . taStem >>= mapM appendConjNum
  where
    taStem :: JConj -> T.Text
    taStem x | not isKanji || null (teFormK x) = T.pack (teForm x)
             | otherwise                       = T.pack (teFormK x)
    gen :: T.Text -> [VForm]
    gen = "て" `replaceSuffix` ["たことがあります", "たり"]
    gen' :: T.Text -> [VForm]
    gen' = "で" `replaceSuffix` ["だことがあります", "だり"]

genNaiForms :: Bool -> JConj -> [VForm]
genNaiForms isKanji = gen . naiStem >>= mapM appendConjNum
  where
    naiStem :: JConj -> T.Text
    naiStem x | not isKanji || null (naiFormK x) = T.pack (naiForm x)
              | otherwise                        = T.pack (naiFormK x)
    gen :: T.Text -> [VForm]
    gen = "ない" `replaceSuffix` ["ないでください", "なくてもいいです", "なければなりません"]

infixl 3 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

generateForms :: Foldable t => Bool -> t [JConj] -> [T.Text]
generateForms isKanjiAlways = foldr go []
  where
    isKanji :: JConj -> Bool
    isKanji = (isKanjiAlways ||) <$> inConjTags "kanji"
    go :: [JConj] -> [T.Text] -> [T.Text]
    go = flip $ foldr ((++) . go')
    go' :: JConj -> [T.Text]
    go' = do
        b  <- isKanji
        vs <-
            genDictForms b
            <++> genMasuForms b
            <++> genTeForms b
            <++> genTaForms b
            <++> genNaiForms b
        return (map vFormToText vs)

putStrUtf8 :: T.Text -> IO ()
putStrUtf8 = BS.putStr . T.encodeUtf8 . (`T.append` "\n")

randomWrite :: FilePath -> [T.Text] -> IO ()
randomWrite fn xs = shuffleM xs >>= T.writeFile fn . T.unlines

main :: IO ()
main = do
    mconj <- T.decodeFileL "../conjugations.txt" >>= either
        (\e -> error $ "Can't parse JConj table " ++ e)
        (return . buildMap conjNumber)
    checkMap mconj
    let conjFormsQ = generateForms False mconj
        conjFormsA = generateForms True mconj
    T.writeFile "../test-forms.txt"  (T.unlines conjFormsQ)
    T.writeFile "../test-formsA.txt" (T.unlines conjFormsA)
    (randFormsQ, randFormsA) <- fmap unzip . shuffleM $ zip conjFormsQ
                                                            conjFormsA
    T.writeFile "../random-formsQ.txt" (T.unlines randFormsQ)
    T.writeFile "../random-formsA.txt" (T.unlines randFormsA)

