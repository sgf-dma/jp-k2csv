{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TupleSections          #-}

module Main where

import           Data.Maybe
import           Data.Either
import           Data.Tuple
import qualified Data.List              as L
import           Data.List.Extra (snoc)
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as BS
import qualified Data.Attoparsec.Text   as A
import           Control.Applicative
import           Control.Arrow
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
--
-- FIXME: If last matching word ends at `wordSep` it won't be found. [parser]
-- FIXME: If wordEnd encountered before first match, no further words are
-- searched. [parser]
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

--gen = "" `replaceSuffix` ["前に", "ことができます"]

--gen = "ます" `replaceSuffix` ["たい", "たくない", "たかった", "たくなかった"]

--gen = "て" `replaceSuffix` ["てください", "てもいいです", "てはいけません", "ています"]

--gen = "て" `replaceSuffix` ["たことがあります", "たり"]

--gen = "ない" `replaceSuffix` ["ないでください", "なくてもいいです", "なければなりません"]


type Writing        = [T.Text]

writingToLine :: [T.Text] -> T.Text
writingToLine = T.concat . L.intersperse ", "

data VForm2         = VForm2
                        { kanaForm2     :: Writing
                        , kanjiForm2    :: Writing
                        , translForm2   :: Writing
                        }
  deriving (Show)

data VFormSpec       = VFormSpec
                        { stem :: JConj -> VForm2
                        , newSuf :: T.Text
                        , transMod :: T.Text -> T.Text
                        }

defVFormSpec :: VFormSpec
defVFormSpec        = VFormSpec
                        { stem = undefined
                        , newSuf = ""
                        , transMod = id
                        }

-- FIXME: Complete table.
voicedChars :: [(Char, Char)]
voicedChars     = [ ( 'て', 'で'), ( 'た', 'だ' ), ('T', 'D') ]

-- | Given a character (either voiceless or voiced) return a pair, where first
-- is voiceless and second is voiced character versions. If given character
-- does not have voiced version, return `Nothing`.
voicedPair :: Char -> Maybe (Char, Char)
voicedPair x    =       (x ,) <$> lookup x voicedChars
                    <|> (, x) <$> lookup x (map swap voicedChars)

voicedTPair :: T.Text -> (T.Text, T.Text)
voicedTPair t
  | T.null t    = (t, t)
  | otherwise   = maybe (t, t) (both (`T.cons` T.tail t))
                    $ voicedPair (T.head t)

maybeNotEmpty :: [a] -> Maybe [a]
maybeNotEmpty xs
  | null xs     = Nothing
  | otherwise   = Just xs

both :: Arrow a => a b c -> a (b, b) (c, c)
both f  = f *** f

teBased :: T.Text -> JConj -> VForm2
teBased suf w =
    VForm2
        { kanaForm2     = gen . kanaStem $ w
        , kanjiForm2    = gen . kanjiStem $ w
        , translForm2   = [T.pack . conjTranslate $ w]
        }
  where
    kanaStem :: JConj -> T.Text
    kanaStem    = T.pack . teForm
    kanjiStem :: JConj -> T.Text
    kanjiStem | null (teFormK w)  = T.pack . teForm
              | otherwise         = T.pack . teFormK
    gen :: T.Text -> Writing
    gen t      = let (uSuf, vSuf) = voicedTPair suf
                  in  fromMaybe [] $
                            map (`T.append` uSuf) <$> genU t
                        <|> map (`T.append` vSuf) <$> genV t
    -- | Split to words using unvocied suffix.
    genU :: T.Text -> Maybe [T.Text]
    genU    = maybeNotEmpty . wordsWithSuffix "て"
    -- | Split to words using voiced suffix.
    genV :: T.Text -> Maybe [T.Text]
    genV    = maybeNotEmpty . wordsWithSuffix "で"

naiBased :: T.Text -> JConj -> VForm2
naiBased suf w =
    VForm2
        { kanaForm2     = gen . T.pack . naiForm $ w
        , kanjiForm2    = gen . T.pack . kanjiStem $ w
        , translForm2   = [T.pack . conjTranslate $ w]
        }
  where
    kanjiStem :: JConj -> String
    kanjiStem y | null (naiFormK w) = naiForm y
                | otherwise         = naiFormK y
    gen :: T.Text -> Writing
    gen     = map (`T.append` suf) . wordsWithSuffix "ない"

dictBased :: T.Text -> JConj -> VForm2
dictBased suf w =
    VForm2
        { kanaForm2     = gen . T.pack . dictForm $ w
        , kanjiForm2    = gen . T.pack . kanjiStem $ w
        , translForm2   = [T.pack . conjTranslate $ w]
        }
  where
    kanjiStem :: JConj -> String
    kanjiStem y | null (dictFormK w) = dictForm y
                | otherwise          = dictFormK y
    gen :: T.Text -> [T.Text]
    gen     = map (`T.append` suf) . wordsWithSuffix ""

masuBased :: T.Text -> JConj -> VForm2
masuBased suf w =
    VForm2
        { kanaForm2     = gen . T.pack . masuForm $ w
        , kanjiForm2    = gen . T.pack . kanjiStem $ w
        , translForm2   = [T.pack . conjTranslate $ w]
        }
  where
    kanjiStem :: JConj -> String
    kanjiStem y | null (masuFormK w) = masuForm y
                | otherwise          = masuFormK y
    gen :: T.Text -> [T.Text]
    gen     = map (`T.append` suf) . wordsWithSuffix "ます"


masuSpec :: [VFormSpec]
masuSpec    = [ defVFormSpec {stem = masuBased "ます", transMod = id} ]

dictSpec :: [VFormSpec]
dictSpec    = [ defVFormSpec {stem = dictBased "", transMod = id} ]

taSpec  :: [VFormSpec]
taSpec      = [ defVFormSpec {stem = teBased "た", transMod = id} ]

taSpec'   :: [VFormSpec]
taSpec'     = [ defVFormSpec {stem = teBased "だ", transMod = id} ]

naiSpec  :: [VFormSpec]
naiSpec     = [ defVFormSpec {stem = naiBased "ない", transMod = id} ]

nakattaSpec :: [VFormSpec]
nakattaSpec = [ defVFormSpec {stem = naiBased "なかった", transMod = id} ]

futsuuSpec :: [VFormSpec]
futsuuSpec  = dictSpec ++ naiSpec ++ taSpec ++ nakattaSpec

{-oldVFCompat :: [VFormSpec]
oldVFCompat =  [ VFormSpec {stem = dictStem, newSuf = "前に", transMod = id}
        , VFormSpec {stem = dictStem, newSuf = "ことができます", transMod = id}
        , VFormSpec {stem = masuStem, newSuf = "ます", transMod = id}
        , VFormSpec {stem = masuStem, newSuf = "たい", transMod = id}
        , VFormSpec {stem = masuStem, newSuf = "たくない", transMod = id}
        , VFormSpec {stem = masuStem, newSuf = "たかった", transMod = id}
        , VFormSpec {stem = masuStem, newSuf = "たくなかった", transMod = id}
        , VFormSpec {stem = teStem  , newSuf = "てください", transMod = id}
        , VFormSpec {stem = teStem  , newSuf = "てもいいです", transMod = id}
        , VFormSpec {stem = teStem  , newSuf = "てはいけません", transMod = id}
        , VFormSpec {stem = teStem  , newSuf = "ています", transMod = id}
        , VFormSpec {stem = teStem' , newSuf = "でください", transMod = id}
        , VFormSpec {stem = teStem' , newSuf = "でもいいです", transMod = id}
        , VFormSpec {stem = teStem' , newSuf = "ではいけません", transMod = id}
        , VFormSpec {stem = teStem' , newSuf = "でいます", transMod = id}
        , VFormSpec {stem = teStem  , newSuf = "たことがあります", transMod = id}
        , VFormSpec {stem = teStem  , newSuf = "たり", transMod = id}
        , VFormSpec {stem = teStem' , newSuf = "だことがあります", transMod = id}
        , VFormSpec {stem = teStem' , newSuf = "だり", transMod = id}
        , VFormSpec {stem = naiStem , newSuf = "ないでください", transMod = id}
        , VFormSpec {stem = naiStem , newSuf = "なくてもいいです", transMod = id}
        , VFormSpec {stem = naiStem , newSuf = "なければなりません", transMod = id}
        ]-}

genSpec :: VFormSpec -> JConj -> VForm2
genSpec VFormSpec {..} x =
    let v@(VForm2 {..}) = stem x
        vn = conjNumber x
    in  v
            { kanaForm2 =
                if null kanaForm2 then []
                    else flip snoc (T.pack . show $ vn) . map (`T.append` newSuf) $ kanaForm2
            , kanjiForm2 =
                if null kanjiForm2 then []
                    else flip snoc (T.pack . show $ vn) . map (`T.append` newSuf) $ kanjiForm2
            }

genSpec' :: VFormSpec -> JConj -> VForm2
genSpec' VFormSpec {..} x =
    let v@(VForm2 {..}) = stem x
    in  v
            { kanaForm2 =
                if null kanaForm2 then []
                    else kanaForm2
            , kanjiForm2 =
                if null kanjiForm2 then []
                    else kanjiForm2
            }

generateForms2 :: Foldable t => [VFormSpec] -> Bool -> t [JConj] -> [T.Text]
generateForms2 vsp isKanjiAlways = foldr go []
  where
    isKanji :: JConj -> Bool
    isKanji = (isKanjiAlways ||) <$> inConjTags "kanji"
    go :: [JConj] -> [T.Text] -> [T.Text]
    go = flip $ foldr ((++) . filter (not . T.null) . go')
    go' :: JConj -> [T.Text]
    go'   = do
        b  <- isKanji
        vs <- mapM genSpec vsp
        if b
          then return (map (writingToLine . kanjiForm2) $ vs)
          else return (map (writingToLine . kanaForm2)  $ vs)

-- | Answer is always "full" (contains all forms), thus the answer should also
-- be on a single line. But questions may be different and each may contain a
-- different number of forms.
data RunSpec        = RunSpec
                        { questionSpec  :: [LineSpec]
                        , questionWriting :: JConj -> VForm2 -> Writing
                        , answerSpec    :: LineSpec
                        , answerWriting :: JConj -> VForm2 -> Writing
                        }
defRunSpec :: RunSpec
defRunSpec      = RunSpec
                        { questionSpec      = []
                        , questionWriting   = isKanji False
                        , answerSpec        = LineSpec []
                        , answerWriting     = isKanji True
                        }

-- Forms, which should be output on a single line.
data LineSpec       = LineSpec {lineSpec :: [VFormSpec]}

genLine :: LineSpec -> (VForm2 -> Writing) -> JConj -> T.Text
genLine (LineSpec vsp) f    = do
    vs <- mapM genSpec' vsp
    jn <- conjNumber
    return . T.concat . L.intersperse "; "
        . filter (not . T.null)
        . flip snoc (T.pack . show $ jn) . map (writingToLine . f)
        $ vs

genLine' :: [LineSpec] -> (VForm2 -> Writing) -> JConj -> [T.Text]
genLine' lsp f x  = map (\l -> genLine l f x) lsp

zipM :: Monad m => m [a] -> m [b] -> m [(a, b)]
zipM mxs mys    = do
    xs <- mxs
    ys <- mys
    return (zip xs ys)

generateForms4 :: Foldable t => RunSpec -> t [JConj] -> [(T.Text, T.Text)]
generateForms4 RunSpec{..} = foldr ((++) . go) []
  where
    questions :: JConj -> [T.Text]
    questions   = questionWriting >>= genLine' questionSpec
    answer :: JConj -> T.Text
    answer  = answerWriting >>= genLine answerSpec
    go :: [JConj] -> [(T.Text, T.Text)]
    go = concatMap (zipM questions (sequence (repeat answer)))

infixl 3 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

putStrUtf8 :: T.Text -> IO ()
putStrUtf8 = BS.putStr . T.encodeUtf8 . (`T.append` "\n")

randomWrite :: FilePath -> [T.Text] -> IO ()
randomWrite fn xs = shuffleM xs >>= T.writeFile fn . T.unlines

writeVerbFiles :: String -> ([T.Text], [T.Text]) -> IO ()
writeVerbFiles fnSuf (conjFormsQ, conjFormsA) = do
    let qfn  = "../wonly-formsQ" ++ fnSuf ++ ".txt"
        qrfn = "../wonly-random-formsQ" ++ fnSuf ++ ".txt"
        afn  = "../wonly-formsA" ++ fnSuf ++ ".txt"
        arfn = "../wonly-random-formsA" ++ fnSuf ++ ".txt"
    T.writeFile qfn  (T.unlines conjFormsQ)
    T.writeFile afn (T.unlines conjFormsA)
    (randFormsQ, randFormsA) <- fmap unzip . shuffleM $ zip conjFormsQ
                                                            conjFormsA
    T.writeFile qrfn (T.unlines randFormsQ)
    T.writeFile arfn (T.unlines randFormsA)

data LNum       = LNum {lessonNum :: Int, seqNum :: Int}
  deriving (Show)

lnumP :: A.Parser LNum
lnumP = LNum <$> (A.string "M" *> A.decimal <* "-") <*> (A.string "W" *> A.decimal)

conjLNums :: JConj -> [LNum]
conjLNums = rights . map (A.parseOnly lnumP) . toWords . T.pack . conjReference

inConjLnums :: (LNum -> Bool) -> JConj -> Bool
inConjLnums p = any p . conjLNums

isKanji :: Bool -> JConj -> VForm2 -> Writing
isKanji isKanjiAlways = (\b -> if b then kanjiForm2 else kanaForm2) . (isKanjiAlways ||)
            <$> inConjTags "kanji"

ddd :: RunSpec
ddd     = defRunSpec
            { questionSpec = [LineSpec dictSpec, LineSpec taSpec]
            , answerSpec = LineSpec futsuuSpec
            }

masuDisctRS :: RunSpec
masuDisctRS = defRunSpec
            { questionSpec = [LineSpec masuSpec]
            , answerSpec = LineSpec dictSpec
            }

futsuuRS :: RunSpec
futsuuRS = defRunSpec
            { questionSpec = map (LineSpec . (: [])) futsuuSpec
            , answerSpec = LineSpec futsuuSpec
            }

masuFutsuuRS :: RunSpec
masuFutsuuRS = defRunSpec
            { questionSpec = [LineSpec masuSpec]
            , answerSpec = LineSpec futsuuSpec
            }

-- FIXME: Rewrite `oldVFRS` to properly generate ta/da verb forms.
{-oldVFRS :: RunSpec
oldVFRS         = RunSpec
            { questionSpec = map (LineSpec . (: [])) oldVFCompat
            , questionWriting = isKanji False
            , answerSpec = LineSpec masuSpec
            , answerWriting = isKanji True
            }-}

taRS :: RunSpec
taRS    = defRunSpec
            { questionSpec = [LineSpec taSpec]
            , answerSpec = LineSpec masuSpec
            }

taRS' :: RunSpec
taRS'   = defRunSpec
            { questionSpec = [LineSpec taSpec']
            , answerSpec = LineSpec masuSpec
            }

nakattaTaRS :: RunSpec
nakattaTaRS = defRunSpec
            { questionSpec = [LineSpec nakattaSpec]
            , answerSpec = LineSpec taSpec
            }


main :: IO ()
main = do
    mconj' <- T.decodeFileL "../conjugations.txt" >>= either
        (\e -> error $ "Can't parse JConj table " ++ e)
        (return . buildMap conjNumber)
    checkMap mconj'
    let mconj = M.filter (any (inConjLnums (const True))) mconj'

    writeVerbFiles "-futsuu2"   ( generateForms2 futsuuSpec False mconj
                                , generateForms2 futsuuSpec True mconj
                                )

    writeVerbFiles "-ddd2" (unzip $ generateForms4 ddd mconj)
    --writeVerbFiles "-oldVF" (unzip $ generateForms4 oldVFRS mconj)
    writeVerbFiles "-dict4-2" ( unzip $ generateForms4 masuDisctRS mconj)
    -- FIXME: Is not equivalent of `futsuu`.
    writeVerbFiles "-futsuu4-2" ( unzip $ generateForms4 futsuuRS mconj)
    writeVerbFiles "-futsuu41-2" ( unzip $ generateForms4 masuFutsuuRS mconj)
    writeVerbFiles "-ta2" ( unzip $ generateForms4 taRS mconj)
    writeVerbFiles "-ta2v" ( unzip $ generateForms4 taRS' mconj)
    writeVerbFiles "-nakattaTa2" ( unzip $ generateForms4 nakattaTaRS mconj)

