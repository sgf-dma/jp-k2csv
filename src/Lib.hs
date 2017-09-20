{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Lib
    ( table
    ) where

import Data.Typeable
import Data.String
import Data.Char
import Data.Data
import Data.Monoid
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
wordWspace          = T.cons
                        <$> A.satisfy (/= '\n')
                        <*> A.takeWhile (\x -> all (/= x) [' ', '\n'])

whenNotP ::    A.Parser a -- ^ Predicate.
            -> A.Parser b -- ^ Parser to run.
            -> A.Parser b
whenNotP p x        = do
    r <- A.eitherP p x
    case r of
      Left  _   -> fail "Predicate does _not_ fail in `whenNotP`."
      Right y   -> return y

data Tt          = Tt
                        { number        :: Int
                        , name      :: T.Text
                        , addr      :: T.Text
                        , phone      :: T.Text
                        }
  deriving (Show, Read)

-- | Cell row /not/ including spaces around cell separators. Version
-- /requiring/ starting and ending @|@ character.
cellLine :: A.Parser T.Text
cellLine            = T.concat <$>
    (   A.string "| "
    *>  some (whenNotP (A.string " |") wordWspace)
    <*  A.string " ")

rowLine :: Ord a => [a] -> A.Parser (M.Map a T.Text)
rowLine ks          = M.fromList . zip ks <$>
    (   some cellLine
    <*  A.string "|" <* A.takeTill A.isEndOfLine <* A.endOfLine)

-- | Separator row.
sepRow :: A.Parser [T.Text]
sepRow              =
        some (A.string "+" *> A.takeWhile1 (== '-'))
    <*  A.string "+" <* A.takeTill A.isEndOfLine <* A.endOfLine

headSepRow :: A.Parser [T.Text]
headSepRow          =
        some (A.string "+" *> A.takeWhile1 (== '='))
    <*  A.string "+" <* A.takeTill A.isEndOfLine <* A.endOfLine

-- | Multi-line table row. Newline at the end is /required/.
-- FIXME: Make it not required. May be replace `endOfLine` with some parser
-- `tokEnd`, which will match with `endOfInput` too.
-- FIXME: Validate, that separator and row lengthes match.
row :: [TableKey] -> A.Parser (M.Map TableKey T.Text)
row ks              =
        (foldr (M.unionWith unlines') M.empty <$> some (rowLine ks))
    <*  sepRow

headerRowN :: A.Parser (M.Map Int T.Text)
headerRowN          =
        sepRow
    *>  (foldr (M.unionWith unlines') M.empty <$> some (rowLine [1..]))
    <*  headSepRow

unlines' :: T.Text -> T.Text -> T.Text
unlines' x y    = x <> "\n" <> y

-- | Parse header line first, then parse header values and use them as
-- 'Map TableKey T.Text' later.
table :: A.Parser TableKey -> A.Parser (M.Map Int (M.Map TableKey T.Text))
table hp  = do
    hm <- headerRowN
    hs <- case mapM (\t -> A.parseOnly hp t) hm of
      Right mx -> return (M.elems mx)
      Left e   -> fail $ "Header parsing failed with " ++ e
    M.fromList . zip [2..] <$> some (row hs)


type TableKey   = T.Text

data Table      = TableInt  (M.Map Int Table)
                | TableText (M.Map T.Text Table)
                | Cell T.Text
  deriving (Show, Data)

type TableParser a  = Either String a

valueType :: Typeable a => TableParser a -> a
valueType _         = undefined

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
    parseTable      = withCell "Text" (A.parseOnly trimWhitespace)

instance FromTable a => FromTable [a] where
    parseTable      = withTableInt "[a]" $
                        foldM (\z -> fmap (: z) . parseTable) []

instance FromTable Tt where
    parseTable      = withTableText "Tt" $ \m ->
                        Tt
                            <$> m .: "Num"
                            <*> m .: "Name"
                            <*> m .: "Address"
                            <*> m .: "Phone"

(.:) :: FromTable a => M.Map T.Text Table -> TableKey -> TableParser a
m .: k              = lookupP parseTable m k

lookupP :: (Table -> TableParser a)
            -> M.Map T.Text Table -> T.Text -> TableParser a
lookupP p o k       = case M.lookup k o of
    Just x  -> p x
    Nothing -> Left $ "No such key: '" ++ show k ++ "'."

tableT :: A.Parser T.Text -> A.Parser Table
tableT hp   = let t = table hp
              in  TableInt . M.map (\x -> TableText (M.map Cell x)) <$> t

row1T :: A.Parser T.Text -> A.Parser Table
row1T hp    = let t = table hp
              in  TableText . M.map Cell . snd . head . M.toList <$> t

table2T :: A.Parser T.Text -> A.Parser Table
table2T hp  = let t = table hp
              in  TableInt . M.singleton 1 .
                    TableInt . M.map (\x -> TableText (M.map Cell x)) <$> t

testT :: IO (Either String [Tt])
testT       = do
    c <- T.readFile "1.txt"
    case A.parseOnly (tableT trimWhitespace) c of
      Right tx -> return (parseTable tx)
      Left  e  -> error $ "Can't parse table with: " ++ e

-- | Does not work due to mismatch of table structure expected from result
-- type and actual. Will work, if result type will be 'Either String Tt'.
testRow1T :: IO (Either String [Tt])
testRow1T   = do
    c <- T.readFile "1.txt"
    case A.parseOnly (row1T trimWhitespace) c of
      Right tx -> return (parseTable tx)
      Left  e  -> error $ "Can't parse table with: " ++ e

-- | Does not work due to mismatch of table structure expected from result
-- type and actual. Will work, if result type will be 'Either String [[Tt]]'.
test2T :: IO (Either String [Tt])
test2T      = do
    c <- T.readFile "1.txt"
    case A.parseOnly (table2T trimWhitespace) c of
      Right tx -> return (parseTable tx)
      Left  e  -> error $ "Can't parse table with: " ++ e



someFunc :: IO ()
someFunc = putStrLn "someFunc"

