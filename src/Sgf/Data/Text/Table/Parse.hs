{-# LANGUAGE OverloadedStrings      #-}

module Sgf.Data.Text.Table.Parse
    ( TableFormat (..)

    , emptyLine
    , row
    , headerRow

    , rawTable
    , rawTables
    , decodeFileRaw

    , whenNotP
    )
  where

import Data.Typeable
import Data.Tagged
import Data.Functor
import Control.Applicative
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Attoparsec.Text   as A

import Sgf.Data.Text.Parse

witnessM :: Functor m => m (Tagged s b) -> m s -> m b
witnessM            = flip $ const (fmap untag)

-- | Read a word consisting from at least one character until a space or
-- newline.
wordWspace :: A.Parser T.Text
wordWspace          = (T.cons
                        <$> A.satisfy (/= '\n')
                        <*> A.takeWhile (\x -> x `notElem` [' ', '\n'])) A.<?> "word"

takeLine :: A.Parser T.Text
takeLine            = A.takeTill A.isEndOfLine <* (A.endOfLine <|> A.endOfInput)

emptyLine :: A.Parser T.Text
emptyLine           =
    A.takeWhile (`elem` [' ', '\t']) <* A.endOfLine A.<?> "emptyLine"

-- | Align marker on left side of a cell.
data SideLeft   = SideLAlign
                | SideLNoAlign
  deriving (Show)

-- | Align marker on right side of a cell.
data SideRight  = SideRAlign
                | SideRNoAlign
  deriving (Show)

-- | Cell alignment.
data CellAlign      = CellRight
                    | CellLeft
                    | CellCenter
                    | CellNoAlign
  deriving (Show)

-- Choose cell align depending on align marker along left and right sides of a
-- cell.
chooseAlign :: SideLeft -> SideRight -> CellAlign
chooseAlign SideLAlign    SideRAlign   = CellCenter
chooseAlign SideLAlign    SideRNoAlign = CellLeft
chooseAlign SideLNoAlign  SideRAlign   = CellRight
chooseAlign SideLNoAlign  SideRNoAlign = CellNoAlign

class TableFormat a where
    emptyTable  :: a
    cell        :: T.Text -> a
    table       :: (Typeable k, Ord k) => [k] -> [a] -> a
    unlinesRow  :: a -> a -> a
    cellSep     :: A.Parser (Tagged a T.Text)
    cellRight   :: A.Parser (Tagged a T.Text)
    cellLeft    :: A.Parser (Tagged a T.Text)

-- | Cell row /not/ including spaces around cell separators. Version
-- /requiring/ starting and ending @|@ character. I need to /require/
-- 'cellEnd' here, so parsing not a proper cell will fail instead of consuming
-- part of input until 'wordWspace' fails (e.g. at 'endOfLine').
cellLine :: TableFormat a => A.Parser a
cellLine            =
    let v = cell . T.concat
            <$> (some (whenNotP ((cellSep <|> cellRight) `witnessM` v) wordWspace)
                 A.<?> "Empty cell.")
            <*  cellRight `witnessM` v
    in  v

rowLine :: (TableFormat t, Typeable a, Ord a) => [a] -> A.Parser t
rowLine ks         =
    let v = table ks <$>
                (cellLeft `witnessM` v *> some cellLine <* takeLine)
            A.<?> "rowLine"
    in  v

-- | Generic table separator row.
gSepRow :: A.Parser a -> Char -> A.Parser [a]
gSepRow cellP sep   = A.char sep *> some (cellP <* A.char sep) <* takeLine

-- | Regular separator row.
sepRow :: A.Parser [T.Text]
sepRow  = gSepRow (A.takeWhile1 (== '-')) '+' A.<?> "sepRow"

headSepRow' :: A.Parser [T.Text]
headSepRow'          =
    gSepRow (A.option "" ":" *> A.takeWhile1 (== '=') <* A.option "" ":") '+'
    A.<?> "headSepRow"

headSepRow :: A.Parser [CellAlign]
headSepRow  =
    gSepRow
        (   chooseAlign
            <$> (A.option SideLNoAlign (A.string ":" $> SideLAlign) <* A.takeWhile1 (== '='))
            <*>  A.option SideRNoAlign (A.string ":" $> SideRAlign)
        )
        '+'
    A.<?> "headSepRow"

-- | Multi-line table row. Newline at the end is /required/.
-- FIXME: Make it not required. May be replace `endOfLine` with some parser
-- `tokEnd`, which will match with `endOfInput` too.
-- FIXME: Validate, that separator and row lengthes match.
row :: (TableFormat t, Typeable a, Ord a) => [a] -> A.Parser t
row ks              =
    (foldr1 unlinesRow <$> some (rowLine ks)) <* sepRow
    A.<?> "row"

headerRow :: TableFormat t => A.Parser t
headerRow           =
       sepRow
    *> (foldr1 unlinesRow <$> some (rowLine ([1..] :: [Int])))
    -- FIXME: Validate, that header separators line has correct length. The
    -- number of`CellAlign` elements in a list is the number of correctly
    -- parsed cells according to header separator line.
    <*  headSepRow
    A.<?> "headerRow"

rawTable :: (Typeable a, Ord a, TableFormat t) => [a] -> A.Parser t
rawTable ks         = table ([1..] :: [Int])
                        <$> ((:) <$> headerRow <*> some (row ks))

decodeFileRaw :: (Typeable a, Ord a, TableFormat t) =>
                [a] -> FilePath -> IO (Either String t)
decodeFileRaw ks f  = A.parseOnly (rawTables ks) <$> T.readFile f

rawTables :: (Typeable a, Ord a, TableFormat t) => [a] -> A.Parser t
rawTables ks        = table ([1..] :: [Int])
                        <$> some (rawTable ks <* many emptyLine)

