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

import Sgf.Data.Text.Table.Parse

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

trimWhitespaceT :: T.Text -> T.Text
trimWhitespaceT     = T.dropWhileEnd isSpace . T.dropWhile isSpace

-- | Parse header line first, then parse header values and use them as
-- 'Map TableKey T.Text' later.
tablePC :: FromTable a => A.Parser (TableParser a)
tablePC    = do
    hm <- headerRow
    hs <- case parseTable hm of
      Right mx -> return mx
      Left e   -> fail $ "Header parsing failed with " ++ e
    parseTable . table ([2..] :: [Int])
        <$> some (row (hs `asTypeOf` [keyType]))

type TableKey   = T.Text

data Table      = TableInt  (M.Map Int Table)
                | TableText (M.Map T.Text Table)
                | Cell T.Text
  deriving (Show, Data)

instance TableFormat Table where
    emptyTable      = Cell ""
    cell            = Cell
    table ks        = case cast ks of
        Just iks -> TableInt . M.fromList . zip iks
        Nothing  -> case cast ks of
          Just tks -> TableText . M.fromList . zip tks
          Nothing  -> error $ "tableC: key type unsupported: " ++ show (typeOf ks)
    unlinesRow (Cell x)         (Cell y)        = Cell $ (trimWhitespaceT x <> " " <> trimWhitespaceT y)
    unlinesRow (TableInt x)     (TableInt y)    = TableInt  $ M.unionWith unlinesRow x y
    unlinesRow (TableText x)    (TableText y)   = TableText $ M.unionWith unlinesRow x y
    unlinesRow _                _               = error "Can't join maps of different type."
    cellSep        = Tagged <$> (A.string " | " A.<?> "cellSep")
    cellRight      = Tagged <$> (A.string " |" A.<?> "cellRight")
    cellLeft       = Tagged <$> (A.string "| " A.<?> "cellLeft")

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

decodeFile :: FromTable a => FilePath -> IO (Either String a)
decodeFile f       = T.readFile f >>= return . join . A.parseOnly tablePC

