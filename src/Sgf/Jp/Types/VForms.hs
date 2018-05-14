{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}

module Sgf.Jp.Types.VForms
    ( Writing
    , VForm2 (..)
    , VFormSpec (..)
    , LineSpec (..)
    , QSpec (..)
    , defQSpec
    , RunSpec (..)
    )
  where

import           Data.Maybe
import           Data.Tuple
import           Data.Yaml
import           Data.Aeson.Types
import qualified Data.Text              as T
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

baseForms :: [(T.Text, T.Text -> JConj -> VForm2)]
baseForms   =   [ ("teBased", teBased)
                , ("naiBased", naiBased)
                , ("dictBased", dictBased)
                , ("masuBased", masuBased)
                ]

data VFormSpec      = VFormSpec { stem :: JConj -> VForm2 }

instance FromJSON VFormSpec where
    parseJSON = withObject "vform" $ \v -> explicitParseField go v "vform"
      where
        go :: Value -> Parser VFormSpec
        go = withObject "Object" $ \v -> do
            r <- v .: "base"
            f <- maybe (fail "Can't find base function") return
                    $ lookup r baseForms
            VFormSpec . f <$> v .: "new"

-- Forms, which should be output on a single line.
data LineSpec       = LineSpec {lineSpec :: [VFormSpec]}

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

data RunSpec = RunSpec {runName :: T.Text, runSpec :: [QSpec]}

instance FromJSON RunSpec where
    parseJSON       = withObject "RunSpec" $ \v -> RunSpec
                        <$> v .: "name"
                        <*> v .: "questions"

isKanji :: Bool -> JConj -> VForm2 -> Writing
isKanji isKanjiAlways = (\b -> if b then kanjiForm2 else kanaForm2) . (isKanjiAlways ||)
            <$> inConjTags "kanji"


maybeNotEmpty :: [a] -> Maybe [a]
maybeNotEmpty xs
  | null xs     = Nothing
  | otherwise   = Just xs

both :: Arrow a => a b c -> a (b, b) (c, c)
both f  = f *** f

