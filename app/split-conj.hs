{-# LANGUAGE OverloadedStrings  #-}

import Data.Char
import Prelude hiding (FilePath)
import Turtle.Prelude
import Turtle.Pattern
import Turtle.Line
import Turtle
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString    as BS
import Data.Monoid
import Data.List.Extra
import Control.Applicative

-- Split by character.
untilChar :: Char -> Pattern T.Text
untilChar c         = star (notChar c) <* char c
splitBy :: Char -> Pattern [T.Text]
splitBy c           = snoc <$> some (untilChar c) <*> chars

-- Split by predicate.
untilChar' :: (Char -> Bool) -> Pattern T.Text
untilChar' p        = star (satisfy (not . p)) <* satisfy p
splitBy' :: (Char -> Bool) -> Pattern [T.Text]
splitBy' p          = snoc <$> some (untilChar' p) <*> chars

utf8Line :: Text -> IO ()
utf8Line            = BS.putStr . T.encodeUtf8 . (`T.append` "\n")

viewUtf8 :: MonadIO m => Shell Line -> m ()
viewUtf8            = flip foldIO $ FoldM
                        (const (utf8Line . lineToText))
                        (return ())
                        return

work :: FilePath -> Shell Line
work f              = do
    l <- input f
    let seps = ['|', '|', '(', ')', '（', '）']
    --case match (splitBy' (`elem` seps)) . lineToText $ l of
    case match (splitBy' (`elem` seps)) . lineToText $ l of
      []        -> return l
      (t : _)   -> do
        let ys = intersperse (T.singleton '|')
                    . filter (T.any (not . isSpace))
                    $ t
        select . textToLines . T.concat $ ("|" : snoc ys "|")

main :: IO ()
main                = do
    output "../test.txt" $ work "../conjugations-2.txt"

{-


data SplitL         = SplitL T.Text Char T.Text
  deriving (Show)

data SplitL2        = SplitL2 T.Text Char
  deriving (Show)

pOpenBrace :: Pattern SplitL2
pOpenBrace          = SplitL2 <$> plus (notChar '(') <*> char '('

f :: [T.Text] -> Pattern [T.Text]
f xs    = do
    y <- plus (notChar '(')
    _ <- char '(' <|> eof *> pure undefined
    return (xs ++ [y])

-- map (T.concat . intersperse (T.singleton '|')) . match (splitBy '(')

pOpenBrace2 :: Pattern SplitL
pOpenBrace2         = SplitL <$> chars1 <*> char '(' <*> chars1

splitLText :: SplitL -> T.Text
splitLText (SplitL x d y)   = x <> T.singleton d <> y

toCols :: SplitL -> SplitL
toCols (SplitL x _ y)   = SplitL x '|' y

test :: IO ()
test = putStr "ない"

-}
