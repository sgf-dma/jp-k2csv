{-# LANGUAGE OverloadedStrings      #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative
import qualified Data.Text              as T
import qualified Data.Attoparsec.Text   as A


cellStart :: Char -> Bool
cellStart           = (== '|')
cellEnd :: Char -> Bool
cellEnd             = (== '|')

wordWspace :: A.Parser T.Text
wordWspace          = T.cons <$> A.satisfy (/= '\n') <*> A.takeWhile (\x -> all (/= x) [' ', '\n'])

whenNotP ::    A.Parser a -- ^ Predicate.
            -> A.Parser b -- ^ Parser to run.
            -> A.Parser b
whenNotP p x        = do
    r <- A.eitherP p x
    case r of
      Left  _   -> fail "Predicate does _not_ fail in `whenNotP`."
      Right y   -> return y

-- | Row /including/ spaces around cell separators.
rowC :: A.Parser [T.Text]
rowC    = A.string "|" *> (some $
                A.takeWhile1 (\x -> all (/= x) ['|', '\n'])
                <* A.string "|")
            <* A.endOfLine

-- | Cell row /not/ including spaces around cell separators. Version
-- /requiring/ starting and ending @|@ character.
cellLine :: A.Parser T.Text
cellLine = T.concat <$> (A.string "| " *> (some $ whenNotP (A.string " |") wordWspace)
                <* (A.string " "))

{-
-- | Does not require starting and ending @|@ character.
cellRow2 :: A.Parser T.Text
cellRow2 = T.concat <$> ((some $ whenNotP (A.string " | ") wordWspace)
            <* (A.string " | " <|> (A.endOfLine *> pure " |")))-}

rowLine :: A.Parser [T.Text]
rowLine = some cellLine <* A.string "|" <* A.takeTill A.isEndOfLine <* A.endOfLine

-- | Separator row.
sepRow :: A.Parser [T.Text]
sepRow = some (A.string "+" *> A.takeWhile1 (== '-')) <* A.string "+" <* A.takeTill A.isEndOfLine <* A.endOfLine

headSepRow :: A.Parser [T.Text]
headSepRow = some (A.string "+" *> A.takeWhile1 (== '=')) <* A.string "+" <* A.takeTill A.isEndOfLine <* A.endOfLine

-- | Multi-line table row. Newline at the end is /required/.
-- FIXME: Make it not required. May be replace `endOfLine` with some parser
-- `tokEnd`, which will match with `endOfInput` too.
row :: A.Parser [[T.Text]]
row     = some rowLine <* sepRow

headerRow :: A.Parser [[T.Text]]
headerRow     = some rowLine <* headSepRow

table :: A.Parser [[[T.Text]]]
table       = (:) <$> headerRow <*> some row

someFunc :: IO ()
someFunc = putStrLn "someFunc"
