{-# LANGUAGE OverloadedStrings  #-}

module Sgf.JPWords.Checks
    ( bothKanjiRefAndRel
    , bothKanjiRefAndRel'
    )
  where

import Data.Maybe
import Turtle

-- Not empty word.
word :: Pattern Text
word                = plus (notChar ' ')

-- Not empty right separator.
sepRight :: Pattern Text
sepRight            = spaces1 <|> eof *> pure " "

-- Match a _single_ ref only. But because 'match' returns all possible
-- matches, i will get a list of all refs in a 'match' result.
anyRef :: Pattern a -> Pattern a
anyRef rf           = spaces
    *> (many (word <* spaces1) *> rf <* many (spaces1 *> word))
    <* spaces

-- Find _all_ refs. Because 'match' returns all possible matches, i'll get
-- more variants with partial matches. Only the first match will contain all
-- refs.
allRefs :: Pattern a -> Pattern [a]
allRefs rf          = spaces *>
    ( fmap catMaybes . some $
        (Just <$> rf <|> word *> pure Nothing) <* sepRight
    )

-- Force pattern failure on empty list.
notEmpty :: [a] -> Pattern [a]
notEmpty px
  | null px         = empty
  | otherwise       = pure px


{-data Ref            = KRef {_lesson :: Int, _seqNum :: Int}
                    | KRel {_baseRef :: Ref, _lesson :: Int,  _seqNum :: Int}
  deriving (Show)-}

data Ref            = LRef {_lesson :: Int}
                    | KRef {_baseRef :: Ref, _seqNum :: Int}
                    | KRel {_baseRef :: Ref, _otherInfo :: Text}
  deriving (Show)

kanjiRefBase :: Pattern Text
kanjiRefBase        = text "M" <> plus digit <> text "-K"

lRefP :: Pattern Ref
lRefP               = LRef <$> (text "M" *> decimal)

kRefBaseP :: Pattern Ref
kRefBaseP           = lRefP <* text "-K"

-- Kanji reference.
kanjiRef :: Pattern Text
kanjiRef            = kanjiRefBase <> plus digit

kRefP :: Pattern Ref
kRefP               = KRef <$> kRefBaseP <*> decimal

-- Related to kanji reference.
kanjiRel :: Pattern Text
kanjiRel            = kanjiRefBase <> option (plus digit)
                        <> text "-" <> option word

kRelP :: Pattern Ref
kRelP               = KRel <$> (kRefP <|> kRefBaseP) <*> (text "-" *> option word)

-- Filter out lines with both kanji reference and kanji relative reference set.
bothKanjiRefAndRel :: Shell Line -> Shell Line
bothKanjiRefAndRel  =   grep (allRefs kanjiRel >>= notEmpty)
                      . grep (allRefs kanjiRef >>= notEmpty)

-- Variant 2.
bothKanjiRefAndRel' :: Shell Line -> Shell Line
bothKanjiRefAndRel' = grep (anyRef kanjiRel) . grep (anyRef kanjiRef)

{-data RWord          = W

-- Different variants of numbers: chapter-num or sequence number.
data RefNum         = RefChN Int Int
                    | RefSeq Int

-- Different variants of references by source.
data Ref            = RefW RefNum (Maybe Ref)
                    | RefK RefNum (Maybe Ref)
                    | RefT RefNum (Maybe Ref)
                    | RefR RefNum (Maybe Ref)

-- But in that case any ref may have any as sub-ref. Is it really true?

data Ref            = RefW  { refCh :: Int
                            , refNum :: Int
                            , refType :: String
                            , ref ..
                            }

Ref   { kanjiNum    :: Int
                            , lessonNum   :: Int
                            }
  deriving (Show)

data El             = El    { kanji :: Char
                            , ref   :: Ref
                            }
  deriving (Show)

readRef :: Pattern Ref
readRef             = Ref <$> decimal <*> ("-" *> decimal)

readEl :: Pattern El
readEl              = El
                        <$> (letter <* space <* chars)
                        <*> ("П" *> readRef <* chars)-}

