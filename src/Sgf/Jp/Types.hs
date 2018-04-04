{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module Sgf.Jp.Types
    ( JKana (..)
    , defJKana
    , JWord (..)
    , defJWord
    , JConj (..)
    , defJConj
    , testJConj
    )
  where

import qualified Data.Text              as T
import           Data.Csv

-- | For 'Serialize'.
import           Control.Monad.State

import qualified Sgf.Data.Text.Table    as T
import           Sgf.Data.Text.OldTable



data JKana          = JKana
                        { hiragana  :: String
                        , katakana  :: String
                        , syllable  :: String
                        , desc      :: String
                        }
  deriving (Show, Read)

defJKana :: JKana
defJKana = JKana {hiragana = "", katakana = "", syllable = "", desc = ""}

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

data JWord          = JWord
                        { number        :: Int
                        , reference     :: String
                        , reading1      :: String
                        , reading2      :: String
                        , origin        :: String
                        , translate     :: String
                        , description   :: String
                        , seeAlso       :: String
                        , tags          :: String
                        }
  deriving (Show, Read)

defJWord :: JWord
defJWord = JWord
    { number      = 0
    , reference   = ""
    , reading1    = ""
    , reading2    = ""
    , origin      = ""
    , translate   = ""
    , description = ""
    , seeAlso     = ""
    , tags        = ""
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
            <*> (T.unpack <$> m T..: "SeeAlso")
            <*> (T.unpack <$> m T..: "Tags")

instance ToRecord JWord where
    toRecord JWord {..} = record
                            [ toField number
                            , toField reference
                            , toField reading1
                            , toField reading2
                            , toField origin
                            , toField translate
                            , toField description
                            , toField seeAlso
                            , toField tags
                            ]

data JConj          = JConj
                        { conjNumber    :: Int
                        , conjReference :: String
                        , dictForm      :: String
                        , dictFormK     :: String
                        , masuForm      :: String
                        , masuFormK     :: String
                        , teForm        :: String
                        , teFormK       :: String
                        , naiForm       :: String
                        , naiFormK      :: String
                        , conjTranslate :: String
                        , conjTags      :: String
                        }
  deriving (Show, Read)

defJConj :: JConj
defJConj = JConj
    { conjNumber    = 0
    , conjReference = ""
    , dictForm      = ""
    , dictFormK     = ""
    , masuForm      = ""
    , masuFormK     = ""
    , teForm        = ""
    , teFormK       = ""
    , naiForm       = ""
    , naiFormK      = ""
    , conjTranslate = ""
    , conjTags      = ""
    }
testJConj :: JConj
testJConj = JConj
    { conjNumber    = 1111
    , conjReference = "M-Test-"
    , dictForm      = "Dict-form"
    , dictFormK     = "Dict-kanji-form"
    , masuForm      = "Masu-form-ます"
    , masuFormK     = "Masu-kanji-form-ます"
    , teForm        = "Te-form-て"
    , teFormK       = "Te-kanji-form-て, Te-kanji-form2-て"
    , naiForm       = "Nai-form-ない"
    , naiFormK      = "Nai-kanji-form-ない"
    , conjTranslate = "Translate"
    , conjTags      = "test"
    }

instance T.FromTable JConj where
    parseTable      = T.withTableText "JConj" $ \m ->
        JConj
            <$> m T..: "Num"
            <*> (T.unpack <$> m T..: "Reference")
            <*> (T.unpack <$> m T..: "Dict form")
            <*> (T.unpack <$> m T..: "Dict kanji")
            <*> (T.unpack <$> m T..: "ます-form")
            <*> (T.unpack <$> m T..: "ます kanji")
            <*> (T.unpack <$> m T..: "て-form")
            <*> (T.unpack <$> m T..: "て kanji")
            <*> (T.unpack <$> m T..: "ない-form")
            <*> (T.unpack <$> m T..: "ない kanji")
            <*> (T.unpack <$> m T..: "Translation")
            <*> (T.unpack <$> m T..: "Tags")

instance ToRecord JConj where
    toRecord JConj {..} = record
                            [ toField conjNumber
                            , toField conjReference
                            , toField dictForm
                            , toField dictFormK
                            , toField masuForm
                            , toField masuFormK
                            , toField teForm
                            , toField teFormK
                            , toField naiForm
                            , toField naiFormK
                            , toField conjTags
                            ]

