{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Sgf.Data.Text.Table
  where

import Data.Typeable
import Data.Char
import Data.Data
import Data.Monoid
import Data.Tagged
import Data.Foldable (foldrM)
import Control.Applicative
import Control.Monad
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Attoparsec.Text   as A
import qualified Data.Map               as M


cellDecimal :: Integral a => A.Parser a
cellDecimal         =      A.takeWhile isSpace
                        *> A.decimal
                        <* A.takeWhile isSpace <* A.endOfInput

-- | Drop leading and trailing whitespaces.
trimWhitespace :: A.Parser T.Text
trimWhitespace      = T.concat <$>
      (A.takeWhile isSpace
    *> some (T.append
                <$> A.takeWhile isSpace
                <*> A.takeWhile1 (not . isSpace))
    <* A.takeWhile isSpace <* A.endOfInput)

-- | Read a word consisting from at least one character until a space or
-- newline.
wordWspace :: A.Parser T.Text
wordWspace          = (T.cons
                        <$> A.satisfy (/= '\n')
                        <*> A.takeWhile (\x -> all (/= x) [' ', '\n'])) A.<?> "word"

whenNotP ::    A.Parser a -- ^ Predicate.
            -> A.Parser b -- ^ Parser to run.
            -> A.Parser b
whenNotP p x        = do
    r <- A.eitherP p x
    case r of
      Left  _   -> fail "Predicate does _not_ fail in `whenNotP`."
      Right y   -> return y

cellSep :: A.Parser T.Text
cellSep             = A.string " | " A.<?> "cellSep"
cellRight :: A.Parser T.Text
cellRight           = A.string " |" A.<?> "cellRight"
cellLeft :: A.Parser T.Text
cellLeft            = A.string "| " A.<?> "cellLeft"

takeLine :: A.Parser T.Text
takeLine            = A.takeTill A.isEndOfLine <* (A.endOfLine <|> A.endOfInput)

-- | Cell row /not/ including spaces around cell separators. Version
-- /requiring/ starting and ending @|@ character. I need to /require/
-- 'cellEnd' here, so parsing not a proper cell will fail instead of consuming
-- part of input until 'wordWspace' fails (e.g. at 'endOfLine').
cellLine :: A.Parser T.Text
cellLine            = T.concat <$>
    (some (whenNotP (cellSep <|> cellRight) wordWspace) A.<?> "Empty cell.")
    <* cellRight

cellLine2 :: A.Parser Table
cellLine2            = cell . T.concat <$>
    (some (whenNotP (cellSep <|> cellRight) wordWspace) A.<?> "Empty cell.")
    <* cellRight

witnessM :: Functor m => m (Tagged s b) -> m s -> m b
witnessM            = flip $ const (fmap untag)

cellLineC :: TableFormat a => A.Parser a
cellLineC           =
    let v = cellC . T.concat
            <$> (some (whenNotP ((cellSepC <|> cellRightC) `witnessM` v) wordWspace)
                 A.<?> "Empty cell.")
            <*  cellRightC `witnessM` v
    in  v

sepCell :: Char -> Char -> A.Parser T.Text
sepCell sep cell    = A.takeWhile1 (== cell) <* A.string (T.singleton sep)

rowLine :: Ord a => [a] -> A.Parser (M.Map a T.Text)
rowLine ks          = M.fromList . zip ks <$>
    (cellLeft *> some cellLine <* takeLine) A.<?> "rowLine"

rowLine2 :: (Typeable a, Ord a) => [a] -> A.Parser Table
rowLine2 ks         = table2 ks <$>
    (cellLeft *> some cellLine2 <* takeLine) A.<?> "rowLine"

rowLineC :: (TableFormat t, Typeable a, Ord a) => [a] -> A.Parser t
rowLineC ks         =
    let v = tableC ks <$>
                (cellLeftC `witnessM` v *> some cellLineC <* takeLine)
            A.<?> "rowLine"
    in  v

-- | Separator row.
sepRow :: A.Parser [T.Text]
sepRow              =
    A.string "+" *> some (sepCell '+' '-') <* takeLine A.<?> "sepRow"

headSepRow :: A.Parser [T.Text]
headSepRow          =
    A.string "+" *> some (sepCell '+' '=') <* takeLine A.<?> "headSepRow"

-- | Multi-line table row. Newline at the end is /required/.
-- FIXME: Make it not required. May be replace `endOfLine` with some parser
-- `tokEnd`, which will match with `endOfInput` too.
-- FIXME: Validate, that separator and row lengthes match.
row :: Ord a => [a] -> A.Parser (M.Map a T.Text)
row ks              =
    (foldr (M.unionWith unlines'2) M.empty <$> some (rowLine ks)) <* sepRow
    A.<?> "row"

row2 :: (Typeable a, Ord a) => [a] -> A.Parser Table
row2 ks             =
    (foldr1 unlines'2T <$> some (rowLine2 ks)) <* sepRow
    A.<?> "row"

rowC :: (TableFormat t, Typeable a, Ord a) => [a] -> A.Parser t
rowC ks             =
    (foldr1 unlinesC <$> some (rowLineC ks)) <* sepRow
    A.<?> "row"

headerRowN :: A.Parser (M.Map Int T.Text)
headerRowN          =
       sepRow
    *> (foldr (M.unionWith unlines'2) M.empty <$> some (rowLine [1..]))
    <*  headSepRow
    A.<?> "headerRow"

headerRowN2 :: A.Parser Table
headerRowN2         =
       sepRow
    *> (foldr1 unlines'2T <$> some (rowLine2 ([1..] :: [Int])))
    <*  headSepRow
    A.<?> "headerRow"

headerRowNC :: TableFormat t => A.Parser t
headerRowNC         =
       sepRow
    *> (foldr1 unlinesC <$> some (rowLineC ([1..] :: [Int])))
    <*  headSepRow
    A.<?> "headerRow"

trimWhitespaceT :: T.Text -> T.Text
trimWhitespaceT     = T.dropWhileEnd isSpace . T.dropWhile isSpace

unlines' :: T.Text -> T.Text -> T.Text
unlines' x y    = trimWhitespaceT x <> "\n" <> trimWhitespaceT y

unlines'2 :: T.Text -> T.Text -> T.Text
unlines'2 x y   = trimWhitespaceT x <> " " <> trimWhitespaceT y

unlines'2T :: Table -> Table -> Table
unlines'2T (Cell x)         (Cell y)        = Cell $ unlines'2 x y
unlines'2T (TableInt x)     (TableInt y)    = TableInt  $ M.unionWith unlines'2T x y
unlines'2T (TableText x)    (TableText y)   = TableText $ M.unionWith unlines'2T x y
unlines'2T _                _               = error "Can't join maps of different type."

-- | Parse header line first, then parse header values and use them as
-- 'Map TableKey T.Text' later.
tableP :: A.Parser TableKey -> A.Parser (M.Map Int (M.Map TableKey T.Text))
tableP hp  = do
    hm <- headerRowN
    hs <- case mapM (\t -> A.parseOnly hp t) hm of
      Right mx -> return (M.elems mx)
      Left e   -> fail $ "Header parsing failed with " ++ e
    M.fromList . zip [2..] <$> some (row hs)

tableT :: A.Parser T.Text -> A.Parser Table
tableT hp   = let t = tableP hp
              in  TableInt . M.map (\x -> TableText (M.map Cell x)) <$> t

tableP2 :: FromTable a => A.Parser (TableParser a)
tableP2    = do
    hm <- headerRowN2
    hs <- case parseTable hm of
      Right mx -> return mx
      Left e   -> fail $ "Header parsing failed with " ++ e
    parseTable . table2 ([2..] :: [Int])
        <$> some (row2 (hs `asTypeOf` [keyType]))

tablePC :: FromTable a => A.Parser (TableParser a)
tablePC    = do
    hm <- headerRowNC
    hs <- case parseTable hm of
      Right mx -> return mx
      Left e   -> fail $ "Header parsing failed with " ++ e
    parseTable . tableC ([2..] :: [Int])
        <$> some (rowC (hs `asTypeOf` [keyType]))

type TableKey   = T.Text

class TableFormat a where
    table0 :: a
    cellC :: T.Text -> a
    tableC :: (Typeable k, Ord k) => [k] -> [a] -> a
    unlinesC :: a -> a -> a
    cellSepC :: A.Parser (Tagged a T.Text)
    cellRightC :: A.Parser (Tagged a T.Text)
    cellLeftC :: A.Parser (Tagged a T.Text)

instance TableFormat Table where
    cellC           = Cell
    tableC          = table2
    unlinesC        = unlines'2T
    cellSepC        = Tagged <$> cellSep
    cellRightC      = Tagged <$> cellRight
    cellLeftC       = Tagged <$> cellLeft

data Table      = TableInt  (M.Map Int Table)
                | TableText (M.Map T.Text Table)
                | Cell T.Text
  deriving (Show, Data)

cell :: T.Text -> Table
cell            = Cell
table2 :: (Typeable a, Ord a) => [a] -> [Table] -> Table
table2 ks       = case cast ks of
    Just iks -> TableInt . M.fromList . zip iks
    Nothing  -> case cast ks of
      Just tks -> TableText . M.fromList . zip tks
      Nothing  -> error $ "table2: key type unsupported: " ++ show (typeOf ks)

tableText :: M.Map T.Text Table -> Table
tableText       = TableText

type TableParser a  = Either String a

valueType :: Typeable a => TableParser a -> a
valueType _         = undefined

keyType :: TableKey
keyType             = undefined

-- | Pretty print table constructor. Show 'Cell' with its content.
showTable :: Table -> String
showTable x = case x of
            TableInt  _ -> "TableInt"
            TableText _ -> "TableText"
            Cell      t -> "Cell '" ++ T.unpack t ++ "'"

-- | Throw an error in 'TableParser' about constructor mismatch.
constrMismatch :: String -> Table -> TableParser a
constrMismatch expected actual  =
    Left $ "expected " ++ expected ++ ", encountered " ++ name
  where
    name    = case actual of
                TableInt  _ -> "TableInt"
                TableText _ -> "TableText"
                Cell      _ -> "Cell"


-- FIXME: Take 'Table' value too and include it in erro message, if it's a
-- cell. Like `parseAnn :: A.Parser a -> T.Text -> TableParser ` ? Or just
-- `withCell`, `withIntTable` and `withTextTable` ?
-- | Annotate error message with the parser result type.
annotateError :: Typeable a => TableParser a -> TableParser a
annotateError v     = either (Left . annErr) Right v
  where
    annErr :: String -> String
    annErr e        = "Parse eRRor at '" ++ show (typeOf (valueType v))
                        ++ "'" ++ e

-- | Map a function over error message.
mapError :: Typeable a => (String -> String) -> TableParser a -> TableParser a
mapError f v        = either (Left . f) Right v

-- | Parse a 'Cell'.
withCell :: Typeable a => String -> (T.Text -> TableParser a)
            -> Table -> TableParser a
withCell _ f v@(Cell t) =
    annotateError . mapError (\e -> " in " ++ showTable v) $ f t
withCell s _ v          = constrMismatch s v

-- | Parse a 'TableInt' table.
withTableInt :: Typeable a => String -> (M.Map Int Table -> TableParser a)
                -> Table -> TableParser a
withTableInt s f v@(TableInt t) =
    annotateError . mapError (\e -> " in " ++ showTable v ++ ": " ++ e) $ f t
withTableInt s _ v      = constrMismatch s v

-- | Parse a 'TableText' table.
withTableText :: Typeable a => String -> (M.Map T.Text Table -> TableParser a)
                    -> Table -> TableParser a
withTableText s f v@(TableText t) =
    annotateError . mapError (\e -> " in " ++ showTable v ++ ": " ++ e) $ f t
withTableText s _ v     = constrMismatch s v


class Typeable a => FromTable a where
    parseTable :: Table -> TableParser a

instance FromTable Int where
    parseTable      = withCell "Int" (A.parseOnly cellDecimal)

instance FromTable T.Text where
    parseTable      = withCell "Text" (A.parseOnly (A.option "" trimWhitespace))

instance FromTable a => FromTable [a] where
    parseTable      = withTableInt "[a]" $
                        foldrM (\x z -> fmap (: z) $ parseTable x) []

(.:) :: FromTable a => M.Map T.Text Table -> TableKey -> TableParser a
m .: k              = lookupP parseTable m k

lookupP :: (Table -> TableParser a)
            -> M.Map T.Text Table -> T.Text -> TableParser a
lookupP p o k       = case M.lookup k o of
    Just x  -> p x
    Nothing -> Left $ "No such key: '" ++ show k ++ "'."

decode :: FromTable a => T.Text -> Either String a
decode c            = A.parseOnly (tableT trimWhitespace) c >>= parseTable

decodeFile :: FromTable a => FilePath -> IO (Either String a)
decodeFile f        = T.readFile f >>= return . decode

decodeFile2 :: FromTable a => FilePath -> IO (Either String a)
decodeFile2 f       = T.readFile f >>= return . join . A.parseOnly tableP2

decodeFileC :: FromTable a => FilePath -> IO (Either String a)
decodeFileC f       = T.readFile f >>= return . join . A.parseOnly tablePC

