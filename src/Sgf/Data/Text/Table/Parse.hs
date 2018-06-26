{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE StandaloneDeriving           #-}

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

--data CellAlign      = AlignLeft | AlignRight | AlignCenter

data CellAlign2 a where
    CellR :: ARight -> CellAlign2 ARight
    CellL :: ALeft  -> CellAlign2 ALeft
    CellC :: ACenter -> CellAlign2 ACenter
    CellN :: ANoAlign -> CellAlign2 ANoAlign
deriving instance Show a => Show (CellAlign2 a)

type family CA a b where
    -- For v2
    CA ALeft ARight = ACenter
    CA ALeft ANoAlign  = ALeft
    CA ANoAlign ARight     = ARight
    CA ANoAlign ANoAlign    = ANoAlign
{-    -- For v3
    CA BAlign BAlign     = ACenter
    CA BAlign BNoAlign   = ALeft
    CA BNoAlign BAlign  = ARight
    CA BNoAlign BNoAlign  = ANoAlign-}

{-class ALeftC a where

instance ALeftC ALeft where
instance ALeftC ANoAlign where

class ARightC a where

instance ARightC ARight where
instance ARightC ANoAlign where-}

{-class CA2 a where

instance CA2 ALeft where
instance CA2 ARight where
instance CA2 ACenter where-}

data ALeft  = ALeft
  deriving (Show)
data ARight  = ARight
  deriving (Show)
data ACenter = ACenter
  deriving (Show)
data ANoAlign = ANoAlign
  deriving (Show)


{-data BNoAlign   = BNoAlign
data BAlign     = BAlign-}
data SideLeft a where
    SideLeftAlign :: ALeft -> SideLeft ALeft
    SideLeftNoAlign :: ANoAlign -> SideLeft ANoAlign
data SideRight a where
    SideRightAlign :: ARight -> SideRight ARight
    SideRightNoAlign :: ANoAlign -> SideRight ANoAlign

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

--------
{-sepCell :: Char -> Char -> A.Parser T.Text
sepCell sep cell  = A.takeWhile1 (== cell) <* A.string (T.singleton sep)-}

gSepRow :: A.Parser a -> Char -> A.Parser [a]
gSepRow cellP sep   = A.char sep *> some (cellP <* A.char sep) <* takeLine

{-sepRow :: A.Parser [T.Text]
sepRow              =
    A.string "+" *> some (sepCell '+' '-') <* takeLine A.<?> "sepRow"-}

-- | Regular separator row.
sepRow :: A.Parser [T.Text]
sepRow              = gSepRow (A.takeWhile1 (== '-')) '+' A.<?> "sepRow"

{-headSepCell :: Parser T.Text
headSepCell = (A.option "" ":" *> A.takeWhile1 (== '=') <* A.option "" ":")
        <* A.string "+"-}

{-headSepRow :: A.Parser [T.Text]
headSepRow          =
    A.string "+" *> some (sepCell '+' '=') <* takeLine A.<?> "headSepRow"-}

headSepRow :: A.Parser [T.Text]
headSepRow          =
    gSepRow (A.option "" ":" *> A.takeWhile1 (== '=') <* A.option "" ":") '+'
    A.<?> "headSepRow"

{-headSepRow2 :: A.Parser [(Maybe CellAlign, Maybe CellAlign)]
headSepRow2     = gSepRow
    (   (,)
        <$> (A.option Nothing (A.string ":" $> Just AlignLeft) <* A.takeWhile1 (== '='))
        <*> A.option Nothing (A.string ":" $> Just AlignRight)
    )
    '+'-}

headSepRow3 :: A.Parser [(Maybe (CellAlign2 ALeft), Maybe (CellAlign2 ARight))]
headSepRow3     = gSepRow
    (   (,)
        <$> (A.option Nothing (A.string ":" $> Just (CellL ALeft)) <* A.takeWhile1 (== '='))
        <*> A.option Nothing (A.string ":" $> Just (CellR ARight))
    )
    '+'

{-headSepRow2 :: A.Parser [CellAlign]
headSepRow2          =
    gSepRow
        (   A.option "" (A.string ":" *>  pure AlignLeft)
        <*  A.takeWhile1 (== '=')
        *>  A.option "" (A.string ":" *> pure AlignRight)
        )
        '+'
    A.<?> "headSepRow"-}

{-chooseAlign :: (CA2 a, CA2 b) => Maybe (CellAlign2 a) -> Maybe (CellAlign2 b) -> Maybe (CellAlign2 (CA a b))
chooseAlign Nothing Nothing     = Nothing-}
--chooseAlign (Just (CellAlign2 x)) Nothing   = Just (CellAlign2 x)

{-chooseAlign2 :: (ALeftC a, ARightC b) => CellAlign2 a -> CellAlign2 b -> CellAlign2 (CA a b)
--chooseAlign2 :: CellAlign2 a -> CellAlign2 b -> CellAlign2 (CA a b)
chooseAlign2 (CellL ALeft) (CellR ARight)   = CellC ACenter
chooseAlign2 (CellL ALeft) (CellN ANoAlign)    = CellL ALeft
chooseAlign2 (CellN ANoAlign) (CellR ARight)    = CellR ARight
--chooseAlign2 (CellR ANoRight) (CellR ARight)    = CellR ARight
--chooseAlign2 (CellR ARight) _    = CellR ARight-}

chooseAlign3 :: SideLeft a -> SideRight b -> CellAlign2 (CA a b)
chooseAlign3 (SideLeftAlign _) (SideRightAlign _) = CellC ACenter
chooseAlign3 (SideLeftAlign _) (SideRightNoAlign _) = CellL ALeft
chooseAlign3 (SideLeftNoAlign _) (SideRightAlign _) = CellR ARight
chooseAlign3 (SideLeftNoAlign _) (SideRightNoAlign _) = CellN ANoAlign

--chooseAlign3 :: Maybe (CellAlign2 ALeft) -> Maybe (CellAlign2 ARight) -> Maybe (CellAlign2 (CA a b))
-- L R = C
-- N N = N
-- L N = L
-- N R = R

-------

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

