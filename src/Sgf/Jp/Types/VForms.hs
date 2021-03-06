{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ExistentialQuantification          #-}

module Sgf.Jp.Types.VForms
    ( Writing
    , writingToLine
    , VForm2 (..)
    , LNumFilter (..)
    , VFormFilter (..)
    , VFormSpec (..)
    , LineSpec (..)
    , QSpec (..)
    , defQSpec
    , RunSpec (..)
    , FileSpec (..)
    , defFileSpec
    , VFReader (..)

    , dictBased
    )
  where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Tuple
import           Data.Yaml
import           Data.Aeson.Types
import qualified Data.Text              as T
import qualified Data.Map               as M
import           Control.Applicative
import           Control.Arrow
import           Control.Monad

import           Sgf.Jp.Types
import           Sgf.Data.Text.Parse


type Writing        = [T.Text]

-- FIXME: This __must__ be the reverse of `wordsWithSuffix`. I rely on this in
-- `potentialForm`. And that should fixed. I may fix this, if `JConj` will
-- have fields of type `[Writing]` instead of `String`..
writingToLine :: [T.Text] -> T.Text
writingToLine = T.concat . intersperse ", "

data VForm2         = VForm2
                        { kanaForm2     :: Writing
                        , kanjiForm2    :: Writing
                        , translForm2   :: Writing
                        }
  deriving (Show)

-- FIXME: Complete table.
voicedChars :: [(Char, Char)]
voicedChars     = [ ( 'て', 'で'), ( 'た', 'だ' ), ('T', 'D') ]

-- | v1 dict form endings with え .
eForm :: [(Char, T.Text)]
eForm  =
    [ ('う', "え")
    , ('く', "け")
    , ('ぐ', "げ")
    , ('す', "せ")
    , ('つ', "て")
    , ('ぶ', "べ")
    , ('む', "め")
    , ('ぬ', "ね")
    , ('る', "れ")
    ]

-- | v1 dict form endings with お .
oForm :: [(Char, T.Text)]
oForm  =
    [ ('う', "おう")
    , ('く', "こう")
    , ('ぐ', "ごう")
    , ('す', "そう")
    , ('つ', "とう")
    , ('ぶ', "ぼう")
    , ('む', "もう")
    , ('ぬ', "のう")
    , ('る', "ろう")
    ]

aForm :: [(Char, T.Text)]
aForm  =
    [ ('う', "わ")
    , ('く', "か")
    , ('ぐ', "が")
    , ('す', "さ")
    , ('つ', "た")
    , ('ぶ', "ば")
    , ('む', "ま")
    , ('ぬ', "な")
    , ('る', "ら")
    ]

-- | Change v1 verb dict form ending to some other form.
-- FIXME: Should i take `JConj` here to ensure, that i really change dict
-- form?
dictFormTo :: [(Char, T.Text)] -> T.Text -> Maybe T.Text
dictFormTo xs t = do
    let (ts, mc) = (T.dropEnd 1 t, tLastMay t)
    c <- mc
    (ts <>) <$> lookup c xs

-- | Map of v3 verb dictionary form to potential dictionary form.
v3CausativePassiveDict :: [(T.Text, T.Text)]
v3CausativePassiveDict = [("する", "させられる"), ("くる", "こさせられる"), ("来る", "来させられる")]

v3CausativeDict :: [(T.Text, T.Text)]
v3CausativeDict = [("する", "させる"), ("くる", "こさせる"), ("来る", "来させる")]

v3PassiveDict :: [(T.Text, T.Text)]
v3PassiveDict = [("する", "される"), ("くる", "こられる"), ("来る", "来られる")]

v3PotentialDict :: [(T.Text, T.Text)]
v3PotentialDict = [("する", "できる"), ("くる", "こられる"), ("来る", "来られる")]

v3Imperative :: [(T.Text, T.Text)]
v3Imperative = [("する", "しろ"), ("くる", "こい"), ("来る", "来い")]

v3Volational :: [(T.Text, T.Text)]
v3Volational = [("する", "しよう"), ("くる", "こよう"), ("来る", "来よう")]

v3Conditional :: [(T.Text, T.Text)]
v3Conditional = [("する", "すれば"), ("くる", "くれば"), ("来る", "来れば")]

-- | Change v3 verb to some other form.
v3VerbTo :: [(T.Text, T.Text)] -> T.Text -> Maybe T.Text
v3VerbTo xs w   = do
    (old, new) <- find ((`T.isSuffixOf` w) . fst) xs
    (<> new) <$> (old `T.stripSuffix` w)

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

teBased :: T.Text -> JConj -> Maybe VForm2
teBased suf w = pure $
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

naiBased :: T.Text -> JConj -> Maybe VForm2
naiBased suf w = pure $
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

dictBased :: T.Text -> JConj -> Maybe VForm2
dictBased suf w = pure $
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

masuBased :: T.Text -> JConj -> Maybe VForm2
masuBased suf w = pure $
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

potentialForm :: M.Map Int [JConj] -> JConj -> Maybe JConj
potentialForm _ jc = pure $
    jc
        { dictForm  = T.unpack . writingToLine . genV "る" . T.pack . dictForm $ jc
        , dictFormK = T.unpack . writingToLine . genV "る" . T.pack . dictFormK $ jc
        , masuForm  = T.unpack . writingToLine . genV "ます" . T.pack . dictForm $ jc
        , masuFormK = T.unpack . writingToLine . genV "ます" . T.pack . dictFormK $ jc
        , teForm    = T.unpack . writingToLine . genV "て" . T.pack . dictForm $ jc
        , teFormK   = T.unpack . writingToLine . genV "て" . T.pack . dictFormK $ jc
        , naiForm   = T.unpack . writingToLine . genV "ない" . T.pack . dictForm $ jc
        , naiFormK  = T.unpack . writingToLine . genV "ない" . T.pack . dictFormK $ jc
        }
  where
    -- FIXME: Empty list will omit row or.. what?
    genV1 :: T.Text -> T.Text -> Writing
    genV1 sf = mapMaybe (\t -> (<> sf) <$> dictFormTo eForm t)
                . wordsWithSuffix ""
    genV :: T.Text -> T.Text -> Writing
    genV
      | "v1" `elem` conjTags jc = genV1
      | "v2" `elem` conjTags jc = genV2
      | "v3" `elem` conjTags jc = genV3
      | otherwise = error $ "Unknown verb conjugation in potentialForm for " ++ dictForm jc
    genV2 :: T.Text -> T.Text -> Writing
    genV2 sf    = map (<> "られ" <> sf) . wordsWithSuffix "る"
    genV3 :: T.Text -> T.Text -> Writing
    genV3 sf dws   = do
        -- TODO: Replace this with row substitution function. I already have
        -- row for できる , so if i construct correct stem, i may use it. But
        -- for 来られる there is no row.. should i use 'genV2' then?  Or
        -- should i use row substitution instead of entire this function:
        -- replace current row with another one (generated) and use regular
        -- conjugation base function to construct the final form?
        dw <- wordsWithSuffix "" dws
        (old, new) <- filter ((`T.isSuffixOf` dw) . fst) v3PotentialDict
        pw <- (<> new) <$> maybeToList (old `T.stripSuffix` dw)
        map (<> sf) (wordsWithSuffix "る" pw)

passiveForm :: M.Map Int [JConj] -> JConj -> Maybe JConj
passiveForm _ jc = pure $
    jc
        { dictForm  = T.unpack . writingToLine . genV "る" . T.pack . dictForm $ jc
        , dictFormK = T.unpack . writingToLine . genV "る" . T.pack . dictFormK $ jc
        , masuForm  = T.unpack . writingToLine . genV "ます" . T.pack . dictForm $ jc
        , masuFormK = T.unpack . writingToLine . genV "ます" . T.pack . dictFormK $ jc
        , teForm    = T.unpack . writingToLine . genV "て" . T.pack . dictForm $ jc
        , teFormK   = T.unpack . writingToLine . genV "て" . T.pack . dictFormK $ jc
        , naiForm   = T.unpack . writingToLine . genV "ない" . T.pack . dictForm $ jc
        , naiFormK  = T.unpack . writingToLine . genV "ない" . T.pack . dictFormK $ jc
        }
  where
    -- FIXME: Empty list will omit row or.. what?
    genV1 :: T.Text -> T.Text -> Writing
    genV1 sf = mapMaybe (\t -> (<> "れ" <> sf) <$> dictFormTo aForm t)
                . wordsWithSuffix ""
    genV :: T.Text -> T.Text -> Writing
    genV
      | "v1" `elem` conjTags jc = genV1
      | "v2" `elem` conjTags jc = genV2
      | "v3" `elem` conjTags jc = genV3
      | otherwise = error $ "Unknown verb conjugation in potentialForm for " ++ dictForm jc
    genV2 :: T.Text -> T.Text -> Writing
    genV2 sf    = map (<> "られ" <> sf) . wordsWithSuffix "る"
    genV3 :: T.Text -> T.Text -> Writing
    genV3 sf dws   = do
        dw <- wordsWithSuffix "" dws
        (old, new) <- filter ((`T.isSuffixOf` dw) . fst) v3PassiveDict
        pw <- (<> new) <$> maybeToList (old `T.stripSuffix` dw)
        map (<> sf) (wordsWithSuffix "る" pw)

causativeForm :: M.Map Int [JConj] -> JConj -> Maybe JConj
causativeForm _ jc = pure $
    jc
        { dictForm  = T.unpack . writingToLine . genV "る" . T.pack . dictForm $ jc
        , dictFormK = T.unpack . writingToLine . genV "る" . T.pack . dictFormK $ jc
        , masuForm  = T.unpack . writingToLine . genV "ます" . T.pack . dictForm $ jc
        , masuFormK = T.unpack . writingToLine . genV "ます" . T.pack . dictFormK $ jc
        , teForm    = T.unpack . writingToLine . genV "て" . T.pack . dictForm $ jc
        , teFormK   = T.unpack . writingToLine . genV "て" . T.pack . dictFormK $ jc
        , naiForm   = T.unpack . writingToLine . genV "ない" . T.pack . dictForm $ jc
        , naiFormK  = T.unpack . writingToLine . genV "ない" . T.pack . dictFormK $ jc
        }
  where
    -- FIXME: Empty list will omit row or.. what?
    genV1 :: T.Text -> T.Text -> Writing
    genV1 sf = mapMaybe (\t -> (<> "せ" <> sf) <$> dictFormTo aForm t)
                . wordsWithSuffix ""
    genV :: T.Text -> T.Text -> Writing
    genV
      | "v1" `elem` conjTags jc = genV1
      | "v2" `elem` conjTags jc = genV2
      | "v3" `elem` conjTags jc = genV3
      | otherwise = error $ "Unknown verb conjugation in potentialForm for " ++ dictForm jc
    genV2 :: T.Text -> T.Text -> Writing
    genV2 sf    = map (<> "させ" <> sf) . wordsWithSuffix "る"
    genV3 :: T.Text -> T.Text -> Writing
    genV3 sf dws   = do
        dw <- wordsWithSuffix "" dws
        (old, new) <- filter ((`T.isSuffixOf` dw) . fst) v3CausativeDict
        pw <- (<> new) <$> maybeToList (old `T.stripSuffix` dw)
        map (<> sf) (wordsWithSuffix "る" pw)

causativePassiveAltForm :: M.Map Int [JConj] -> JConj -> Maybe JConj
causativePassiveAltForm _ jc = pure $
    jc
        { dictForm  = T.unpack . writingToLine . genV "る" . T.pack . dictForm $ jc
        , dictFormK = T.unpack . writingToLine . genV "る" . T.pack . dictFormK $ jc
        , masuForm  = T.unpack . writingToLine . genV "ます" . T.pack . dictForm $ jc
        , masuFormK = T.unpack . writingToLine . genV "ます" . T.pack . dictFormK $ jc
        , teForm    = T.unpack . writingToLine . genV "て" . T.pack . dictForm $ jc
        , teFormK   = T.unpack . writingToLine . genV "て" . T.pack . dictFormK $ jc
        , naiForm   = T.unpack . writingToLine . genV "ない" . T.pack . dictForm $ jc
        , naiFormK  = T.unpack . writingToLine . genV "ない" . T.pack . dictFormK $ jc
        }
  where
    -- FIXME: Empty list will omit row or.. what?
    genV1 :: T.Text -> T.Text -> Writing
    genV1 sf v
      | T.takeEnd 1 v == "す"   =
            mapMaybe (\t -> (<> "せられ" <> sf) <$> dictFormTo aForm t)
                . wordsWithSuffix ""
                $ v
      | otherwise               =
            mapMaybe (\t -> (<> "され" <> sf) <$> dictFormTo aForm t)
                . wordsWithSuffix ""
                $ v
    genV :: T.Text -> T.Text -> Writing
    genV
      | "v1" `elem` conjTags jc = genV1
      | "v2" `elem` conjTags jc = genV2
      | "v3" `elem` conjTags jc = genV3
      | otherwise = error $ "Unknown verb conjugation in potentialForm for " ++ dictForm jc
    genV2 :: T.Text -> T.Text -> Writing
    genV2 sf    = map (<> "させられ" <> sf) . wordsWithSuffix "る"
    genV3 :: T.Text -> T.Text -> Writing
    genV3 sf dws   = do
        dw <- wordsWithSuffix "" dws
        (old, new) <- filter ((`T.isSuffixOf` dw) . fst) v3CausativePassiveDict
        pw <- (<> new) <$> maybeToList (old `T.stripSuffix` dw)
        map (<> sf) (wordsWithSuffix "る" pw)

potentialBased :: T.Text -> JConj -> Maybe VForm2
potentialBased suf w
  | "v1" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV1 . T.pack . dictForm $ w
            , kanjiForm2    = genV1 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v2" `elem` conjTags w = pure $

        VForm2
            { kanaForm2     = genV2 . T.pack . dictForm $ w
            , kanjiForm2    = genV2 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v3" `elem` conjTags w = pure $

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
    genV1   = mapMaybe (\t -> (<> suf) <$> dictFormTo eForm t)
                . wordsWithSuffix ""
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

imperativeBased :: T.Text -> JConj -> Maybe VForm2
imperativeBased suf w
  | "noimperative" `elem` conjTags w = mzero
  | "v1" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV1 . T.pack . dictForm $ w
            , kanjiForm2    = genV1 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v2" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV2 . T.pack . dictForm $ w
            , kanjiForm2    = genV2 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v3" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV3 . T.pack . dictForm $ w
            , kanjiForm2    = genV3 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | otherwise = error $ "Unknown verb conjugation for " ++ dictForm w
  where
    kanjiStem :: JConj -> String
    kanjiStem y | null (dictFormK w) = dictForm y
                | otherwise         = dictFormK y
    genV1 :: T.Text -> Writing
    genV1   = mapMaybe (\t -> (<> suf) <$> dictFormTo eForm t)
                . wordsWithSuffix ""
    genV2 :: T.Text -> Writing
    genV2   = map (<> "ろ" <> suf) . wordsWithSuffix "る"
    genV3 :: T.Text -> Writing
    genV3   = mapMaybe (\t -> (<> suf) <$> v3VerbTo v3Imperative t)
                . wordsWithSuffix ""

volitionalBased :: T.Text -> JConj -> Maybe VForm2
volitionalBased suf w
  | "v1" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV1 . T.pack . dictForm $ w
            , kanjiForm2    = genV1 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v2" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV2 . T.pack . dictForm $ w
            , kanjiForm2    = genV2 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v3" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV3 . T.pack . dictForm $ w
            , kanjiForm2    = genV3 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | otherwise = error $ "Unknown verb conjugation for " ++ dictForm w
  where
    kanjiStem :: JConj -> String
    kanjiStem y | null (dictFormK w) = dictForm y
                | otherwise         = dictFormK y
    genV1 :: T.Text -> Writing
    genV1   = mapMaybe (\t -> (<> suf) <$> dictFormTo oForm t)
                . wordsWithSuffix ""
    genV2 :: T.Text -> Writing
    genV2   = map (<> "よう" <> suf) . wordsWithSuffix "る"
    genV3 :: T.Text -> Writing
    genV3   = mapMaybe (\t -> (<> suf) <$> v3VerbTo v3Volational t)
                . wordsWithSuffix ""

conditionalBased :: T.Text -> JConj -> Maybe VForm2
conditionalBased suf w
  | "v1" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV1 . T.pack . dictForm $ w
            , kanjiForm2    = genV1 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v2" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV2 . T.pack . dictForm $ w
            , kanjiForm2    = genV2 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | "v3" `elem` conjTags w = pure $
        VForm2
            { kanaForm2     = genV3 . T.pack . dictForm $ w
            , kanjiForm2    = genV3 . T.pack . kanjiStem $ w
            , translForm2   = [T.pack . conjTranslate $ w]
            }
  | otherwise = error $ "Unknown verb conjugation for " ++ dictForm w
  where
    kanjiStem :: JConj -> String
    kanjiStem y | null (dictFormK w) = dictForm y
                | otherwise         = dictFormK y
    genV1 :: T.Text -> Writing
    genV1   = mapMaybe (\t -> (<> "ば" <> suf) <$> dictFormTo eForm t)
                . wordsWithSuffix ""
    genV2 :: T.Text -> Writing
    genV2   = map (<> "れば" <> suf) . wordsWithSuffix "る"
    genV3 :: T.Text -> Writing
    genV3   = mapMaybe (\t -> (<> suf) <$> v3VerbTo v3Conditional t)
                . wordsWithSuffix ""

baseForms :: [(T.Text, T.Text -> JConj -> Maybe VForm2)]
baseForms   =   [ ("teBased", teBased)
                , ("naiBased", naiBased)
                , ("dictBased", dictBased)
                , ("masuBased", masuBased)
                , ("potentialBased", potentialBased)
                , ("imperativeBased", imperativeBased)
                , ("volitionalBased", volitionalBased)
                , ("conditionalBased", conditionalBased)
                ]

rowModFuncs :: [(T.Text, M.Map Int [JConj] -> JConj -> Maybe JConj)]
rowModFuncs   = [ ("id", const (Just <$> id))
                , ("transPair", lookupTransPair)
                , ("potentialForm", potentialForm)
                , ("passiveForm", passiveForm)
                , ("causativeForm", causativeForm)
                , ("causativePassiveAltForm", causativePassiveAltForm)
                ]

lookupTransPair :: M.Map Int [JConj] -> JConj -> Maybe JConj
lookupTransPair xs v = conjTransRef v >>= flip M.lookup xs >>= listToMaybe

data LNumFilter = LessonRange   {lnumFrom :: Maybe Int, lnumTill :: Maybe Int}
                | Lesson        {lnumEq :: Int}
  deriving (Show)

instance FromJSON LNumFilter where
    parseJSON v     =
            withObject "LNumFilter"
                (\o -> LessonRange <$> o .:? "from" <*> o .:? "till") v
        <|> Lesson <$> parseJSON v

data VFormFilter = VFormFilter  { lFilter   :: Maybe LNumFilter
                                , tagFilter :: [T.Text]
                                }
  deriving (Show)

instance FromJSON VFormFilter where
    parseJSON       = withObject "VFormFilter" $ \o ->
        VFormFilter
            <$> (o .:? "lesson")
            <*> (fromMaybe [] <$> o .:? "tags")

data VFormSpec = VFormSpec
                    { vformBase :: T.Text
                    , stem :: JConj -> Maybe VForm2
                    , vformFilter :: Last VFormFilter
                    , rowMod :: M.Map Int [JConj] -> JConj -> Maybe JConj
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
              <*> (f <$> v .:? "new" .!= "")
              <*> (Last <$> (v .:? "filter"))
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
                , qsSpec    :: [QSpec]
                , runFilter :: Last VFormFilter
                , files     :: FileSpec
                }
  deriving (Show)

instance FromJSON RunSpec where
    parseJSON       = withObject "RunSpec" $ \v -> RunSpec
                        <$> v .: "name"
                        <*> v .: "questions"
                        <*> (Last <$> (v .:? "filter"))
                        <*> v .:? "files" .!= defFileSpec

isKanji :: Bool -> JConj -> VForm2 -> Writing
isKanji isKanjiAlways = (\b -> if b then kanjiForm2 else kanaForm2) . (isKanjiAlways ||)
            <$> inConjTags "kanji"

data VFReader       = VFReader
                        { curJConj :: JConj
                        , jconjMap :: M.Map Int [JConj]
                        , runSpec  :: RunSpec
                        }
  deriving (Show)

maybeNotEmpty :: [a] -> Maybe [a]
maybeNotEmpty xs
  | null xs     = Nothing
  | otherwise   = Just xs

both :: Arrow a => a b c -> a (b, b) (c, c)
both f  = f *** f

