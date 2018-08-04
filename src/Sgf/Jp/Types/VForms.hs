{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ExistentialQuantification          #-}

module Sgf.Jp.Types.VForms
    ( Writing
    , VForm2 (..)
    , VFormSpec (..)
    , LineSpec (..)
    , QSpec (..)
    , defQSpec
    , LNumFilter (..)
    , RunSpec (..)
    , FileSpec (..)
    , defFileSpec
    , VFReader (..)

    , dictBased
    )
  where

import           Data.Maybe
import           Data.Monoid
import           Data.Tuple
import           Data.Yaml
import           Data.Aeson.Types
import qualified Data.Text              as T
import qualified Data.Map               as M
import           Control.Applicative
import           Control.Arrow

import           Sgf.Jp.Types
import           Sgf.Data.Text.Parse


type Writing        = [T.Text]

data VForm2         = VForm2
                        { kanaForm2     :: Writing
                        , kanjiForm2    :: Writing
                        , translForm2   :: Writing
                        }
  deriving (Show)

-- FIXME: Complete table.
voicedChars :: [(Char, Char)]
voicedChars     = [ ( 'て', 'で'), ( 'た', 'だ' ), ('T', 'D') ]

potentialSyllables :: [(Char, Char)]
potentialSyllables  =
    [ ('う', 'え')
    , ('く', 'け')
    , ('ぐ', 'げ')
    , ('す', 'せ')
    , ('つ', 'て')
    , ('ぶ', 'べ')
    , ('む', 'め')
    , ('る', 'れ')
    ]

-- | Map of v3 verb dictionary form to potential dictionary form.
v3PotentialDict :: [(T.Text, T.Text)]
v3PotentialDict = [("する", "できる"), ("くる", "こられる"), ("来る", "来られる")]

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

potentialBased :: T.Text -> JConj -> VForm2
potentialBased suf w
  | "v1" `elem` conjTags w =
        VForm2
            { kanaForm2     = genV1 . T.pack . dictForm $ w
            , kanjiForm2    = genV1 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v2" `elem` conjTags w =
        VForm2
            { kanaForm2     = genV2 . T.pack . dictForm $ w
            , kanjiForm2    = genV2 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v3" `elem` conjTags w =
        VForm2
            { kanaForm2     = genV3 . T.pack . dictForm $ w
            , kanjiForm2    = genV3 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  -- FIXME: Just skip this vform. But his requires result of type 'Maybe'.
  | otherwise = error $ "Unknown verb conjugation for " ++ dictForm w
  where
    kanjiStem :: JConj -> String
    kanjiStem y | null (dictFormK w) = dictForm y
                | otherwise         = dictFormK y
    genV1 :: T.Text -> Writing
    genV1       = mapMaybe go . wordsWithSuffix ""
    genV2 :: T.Text -> Writing
    genV2       = map (<> "られ" <> suf) . wordsWithSuffix "る"
    genV3 :: T.Text -> Writing
    genV3 dws   = do
        -- TODO: Replace this with row substitution function. I already have
        -- row for できる , so if i construct correct stem, i may use it. But
        -- for 来られる there is no row.. should i use 'genV2' then?  Or
        -- should i use row substitution instead of entire this function:
        -- replace current row with another one (generated) and use regular
        -- conjugation base function to construct the final form?
        dw <- wordsWithSuffix "" dws
        (old, new) <- filter ((`T.isSuffixOf` dw) . fst) v3PotentialDict
        pw <- (<> new) <$> maybeToList (old `T.stripSuffix` dw)
        map (<> suf) (wordsWithSuffix "る" pw)
    go :: T.Text -> Maybe T.Text
    go t    = do
        let (ts, mc) = (T.dropEnd 1 t, tLastMay t)
        c <- mc
        (<> suf) . T.snoc ts <$> lookup c potentialSyllables

baseForms :: [(T.Text, T.Text -> JConj -> VForm2)]
baseForms   =   [ ("teBased", teBased)
                , ("naiBased", naiBased)
                , ("dictBased", dictBased)
                , ("masuBased", masuBased)
                , ("potentialBased", potentialBased)
                ]

rowModFuncs :: [(T.Text, M.Map Int [JConj] -> JConj -> Maybe JConj)]
rowModFuncs   = [ ("id", const (Just <$> id))
                , ("transPair", lookupTransPair)
                ]

lookupTransPair :: M.Map Int [JConj] -> JConj -> Maybe JConj
lookupTransPair xs v = conjTransRef v >>= flip M.lookup xs >>= listToMaybe

data VFormSpec = forall m. Foldable m => VFormSpec
                    { vformBase :: T.Text
                    , stem :: JConj -> VForm2
                    , vformFilter :: [T.Text]
                    , rowMod :: m [JConj] -> JConj -> Maybe JConj
                    }

instance Show VFormSpec where
    showsPrec d vs = showParen (d > app_prec)
        $ showString "VFormSpec {"
        . showString "vformBase = " . showsPrec (d + 1) (vformBase vs)
        . showString ", vformFilter = " . showsPrec (d + 1) (vformFilter vs)
        . showString "}"
      where app_prec = 10

instance FromJSON VFormSpec where
    parseJSON = withObject "vform" $ \v -> explicitParseField go v "vform"
      where
        go :: Value -> Parser VFormSpec
        go = withObject "Object" $ \v -> do
            b <- v .: "base"
            -- FIXME: Because now i store 'vformBase' name in 'VFormSpec' i
            -- may move base function lookup out of this module.
            f <- maybe (fail "Can't find base function") return
                    $ lookup b baseForms
            -- FIXME: Better default handling.
            r <- v .:? "rowMod" .!= "id"
            g <- maybe (fail "Can't find row modification function") return
                    $ lookup r rowModFuncs
            VFormSpec
              <$> pure b
              <*> (f <$> v .: "new")
              <*> (v .:? "filter" .!= [])
              <*> pure g

-- Forms, which should be output on a single line.
newtype LineSpec = LineSpec {lineSpec :: [VFormSpec]}
  deriving (Show)

instance FromJSON LineSpec where
    parseJSON   = withObject "line" $ \v -> LineSpec <$> v .: "line"

-- | Answer is always "full" (contains all forms), thus the answer should also
-- be on a single line. But questions may be different and each may contain a
-- different number of forms.
data QSpec        = QSpec
                        { questionSpec  :: [LineSpec]
                        , questionWriting :: JConj -> VForm2 -> Writing
                        , answerSpec    :: LineSpec
                        , answerWriting :: JConj -> VForm2 -> Writing
                        }

instance Show QSpec where
    showsPrec d qs = showParen (d > app_prec)
        $ showString "QSpec {"
        . showString "questionSpec = " . showsPrec (d + 1) (questionSpec qs)
        . showString ", answerSpec = " . showsPrec (d + 1) (answerSpec qs)
        . showString "}"
      where app_prec = 10
defQSpec :: QSpec
defQSpec      = QSpec
                        { questionSpec      = []
                        , questionWriting   = isKanji False
                        , answerSpec        = LineSpec []
                        , answerWriting     = isKanji True
                        }

instance FromJSON QSpec where
    parseJSON     = withObject "question" $ \v -> QSpec
                                <$> v .: "front"
                                <*> pure (isKanji False)
                                <*> v .: "back"
                                <*> pure (isKanji True)

data LNumFilter = LessonRange   {lnumFrom :: Maybe Int, lnumTill :: Maybe Int}
                | Lesson        {lnumEq :: Int}
  deriving (Show)

instance FromJSON LNumFilter where
    parseJSON v     =
            withObject "LNumFilter"
                (\o -> LessonRange <$> o .:? "from" <*> o .:? "till") v
        <|> Lesson <$> parseJSON v

data FileSpec   = FileSpec
                    { destDir   :: FilePath
                    , nfiles    :: Int
                    }
  deriving (Show)

defFileSpec :: FileSpec
defFileSpec = FileSpec
                { destDir = "./vforms"
                , nfiles  = 1
                }

instance FromJSON FileSpec where
    parseJSON       = withObject "FileSpec" $ \v -> FileSpec
                        <$> v .:? "dest"    .!= destDir defFileSpec
                        <*> v .:? "number"  .!= nfiles defFileSpec

data RunSpec = RunSpec
                { runName   :: T.Text
                , runSpec   :: [QSpec]
                , runFilter :: Maybe LNumFilter
                , files     :: FileSpec
                }
  deriving (Show)

instance FromJSON RunSpec where
    parseJSON       = withObject "RunSpec" $ \v -> RunSpec
                        <$> v .: "name"
                        <*> v .: "questions"
                        <*> v .:? "filter" .!= Nothing
                        <*> v .:? "files" .!= defFileSpec

isKanji :: Bool -> JConj -> VForm2 -> Writing
isKanji isKanjiAlways = (\b -> if b then kanjiForm2 else kanaForm2) . (isKanjiAlways ||)
            <$> inConjTags "kanji"

data VFReader       = VFReader {curJConj :: JConj, jconjMap :: M.Map Int [JConj]}
  deriving (Show)

maybeNotEmpty :: [a] -> Maybe [a]
maybeNotEmpty xs
  | null xs     = Nothing
  | otherwise   = Just xs

both :: Arrow a => a b c -> a (b, b) (c, c)
both f  = f *** f

