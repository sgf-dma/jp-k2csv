{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib
    ( someFunc
    ) where

import Data.Typeable
import Data.String
import Data.Char
import Data.Monoid
import Control.Applicative
import Control.Monad
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Attoparsec.Text   as A
import qualified Data.Map               as M


cellStart :: Char -> Bool
cellStart           = (== '|')
cellEnd :: Char -> Bool
cellEnd             = (== '|')

cellDecimal :: Integral a => A.Parser a
cellDecimal         = A.takeWhile isSpace *> A.decimal <* A.takeWhile isSpace <* A.endOfInput

-- | Drop leading and trailing whitespaces.
trimWhitespace :: A.Parser T.Text
trimWhitespace      =
    T.concat <$> (A.takeWhile isSpace
        *> some (T.append <$> A.takeWhile isSpace <*> A.takeWhile1 (not . isSpace))
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

-- | Row /including/ spaces around cell separators.
rowC :: A.Parser [T.Text]
rowC                =
    A.string "|"
        *> (some $ A.takeWhile1 (\x -> all (/= x) ['|', '\n'])
        <*  A.string "|")
    <* A.endOfLine

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
cellLine            =
    T.concat <$>
        (   A.string "| " *> (some $ whenNotP (A.string " |") wordWspace)
        <* (A.string " "))

cellLineP :: A.Parser a -> A.Parser (A.Result a)
cellLineP p          =
    apSubp p <$>
        (   A.string "| " *> (some $ whenNotP (A.string " |") wordWspace)
        <* (A.string " "))

apSubp :: A.Parser a -> [T.Text] -> A.Result a
apSubp p []         = A.parse p ""
apSubp p (x : xs)   = foldl A.feed (A.parse p x) xs

cellTP :: A.Parser Table
cellTP              = undefined

isFail :: A.Result a -> Bool
isFail (A.Fail _ _ _) = True
isFail _            = False

subparser :: A.Parser a -> A.Parser T.Text -> A.Parser (A.Result a)
subparser p t       = do
    r <- A.parse p <$> t
    when (isFail r) (fail "Subparser have failed.")
    return r

{-
-- | Does not require starting and ending @|@ character.
cellRow2 :: A.Parser T.Text
cellRow2 = T.concat <$> ((some $ whenNotP (A.string " | ") wordWspace)
            <* (A.string " | " <|> (A.endOfLine *> pure " |")))-}

rowLine :: A.Parser [T.Text]
rowLine     = some cellLine <* A.string "|" <* A.takeTill A.isEndOfLine <* A.endOfLine

rowLineN :: A.Parser (M.Map Int T.Text)
rowLineN    = M.fromList . zip [1..] <$> rowLine

rowLineK :: [TableKey] -> A.Parser (M.Map TableKey T.Text)
rowLineK ks         = M.fromList . zip ks <$> rowLine

-- | Separator row.
sepRow :: A.Parser [T.Text]
sepRow = some (A.string "+" *> A.takeWhile1 (== '-')) <* A.string "+" <* A.takeTill A.isEndOfLine <* A.endOfLine

headSepRow :: A.Parser [T.Text]
headSepRow = some (A.string "+" *> A.takeWhile1 (== '=')) <* A.string "+" <* A.takeTill A.isEndOfLine <* A.endOfLine

-- | Multi-line table row. Newline at the end is /required/.
-- FIXME: Make it not required. May be replace `endOfLine` with some parser
-- `tokEnd`, which will match with `endOfInput` too.
-- FIXME: Validate, that separator and row lengthes match.
row :: A.Parser [[T.Text]]
row     = some rowLine <* sepRow

rowN :: A.Parser (M.Map Int T.Text)
rowN    = (foldr (M.unionWith unlines') M.empty <$> some rowLineN) <* sepRow

rowNK :: [TableKey] -> A.Parser (M.Map TableKey T.Text)
rowNK ks = (foldr (M.unionWith unlines') M.empty <$> some (rowLineK ks)) <* sepRow

headerRow :: A.Parser [[T.Text]]
headerRow   = sepRow *> some rowLine <* headSepRow

unlines' :: T.Text -> T.Text -> T.Text
unlines' x y    = x <> "\n" <> y

headerRowN :: A.Parser (M.Map Int T.Text)
headerRowN  = sepRow *> (foldr (M.unionWith unlines') M.empty <$> some rowLineN) <* headSepRow

table :: A.Parser [[[T.Text]]]
table           = (:) <$> headerRow <*> some row

--tableN :: A.Parser (M.Map Int (M.Map Int T.Text))
tableN :: A.Parser (M.Map Int (M.Map Int T.Text))
tableN  = (M.fromList . zip [1..]) <$> ((:) <$> headerRowN <*> some rowN)

tableN2 :: A.Parser (M.Map (Int, Int) T.Text)
tableN2 = (M.unions . map (\(r, mr) -> M.mapKeys (\c -> (r, c)) mr) . zip [1..]) <$>
            ((:) <$> headerRowN <*> some rowN)

-- | Parse header line first, then parse header values and use them as
-- 'Map TableKey T.Text' later.
tableNK2 :: A.Parser TableKey -> A.Parser (M.Map Int (M.Map TableKey T.Text))
tableNK2 hp  = do
    hm <- headerRowN
    hs <- case mapM (\t -> A.parseOnly hp t) hm of
      Right mx -> return (M.elems mx)
      Left e   -> fail $ "Header parsing failed with " ++ e
    M.fromList . zip [2..] <$> some (rowNK hs)

-- | Parse table as 'Map Int (Map Int T.Text)`, then parse header row and
-- `mapKeys` for the rest of table.
tableNK :: A.Parser TableKey -> A.Parser (M.Map Int (M.Map TableKey T.Text))
tableNK hp  = do
    mt <- tableN
    let (_, Just hm, mrest) = M.splitLookup 1 mt
    case mapM (\t -> A.parseOnly hp t) hm of
      Right mx -> return (M.map (M.mapKeys (go mx)) mrest)
      Left e   -> fail $ "Header parsing failed with " ++ e
  where
    go :: M.Map Int TableKey -> Int -> TableKey
    go mx i
      | Just k <- M.lookup i mx = k
      | otherwise               = error "No such key"


type TableKey   = T.Text

data Table      = TableInt  (M.Map Int Table)
                | TableText (M.Map T.Text Table)
                | Cell T.Text
  deriving (Show)

class FromTable a where
    parseTable :: Table -> Either String a

instance FromTable a => FromTable [a] where
    parseTable (TableInt t) = foldM (\z x -> parseTable x >>= \r -> return (r : z)) [] t
instance FromTable Tt where
    parseTable (TableText t) = Tt
                            <$> t .: "Num"
                            <*> t .: "Name"
                            <*> t .: "Address"
                            <*> t .: "Phone"
instance FromTable Int where
    parseTable (Cell t) = either (\e -> Left $ "Error in cell " ++ T.unpack t ++ ": " ++ e) Right $ A.parseOnly cellDecimal t
    parseTable _        = error "Type mismatch cell."
instance FromTable T.Text where
    parseTable (Cell t) = A.parseOnly (A.takeWhile isSpace *> A.takeText) t
    parseTable _        = error "Type mismatch at cell."

(.:) :: FromTable a => M.Map T.Text Table -> TableKey -> Either String a
m .: k              = lookupP parseTable m k

lookupP :: (Table -> Either String a) -> M.Map T.Text Table -> T.Text -> Either String a
lookupP p o k       = case M.lookup k o of
    Just x  -> p x
    Nothing -> error $ "No such key: '" ++ show k ++ "'."

tableT :: A.Parser T.Text -> A.Parser Table
tableT hp   = let t = tableNK2 hp
              in  TableInt . M.map (\x -> TableText (M.map Cell x)) <$> t

row1T :: A.Parser T.Text -> A.Parser Table
row1T hp    = let t = tableNK2 hp
              in  TableText . M.map Cell . snd . head . M.toList <$> t

table2T :: A.Parser T.Text -> A.Parser Table
table2T hp  = let t = tableNK2 hp
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


class (Show a, Ord a, Typeable a) => TableKKey a

instance TableKKey T.Text
instance TableKKey Int

data Table2     = forall k. TableKKey k => Table2 (M.Map k Table2)
                | Cell2 T.Text

class FromTable2 a where
    parseTable2 :: Table2 -> Either String a

instance FromTable2 a => FromTable2 [a] where
    parseTable2 (Table2 t)    = foldM (\z x -> parseTable2 x >>= \r -> return (r : z)) [] t
instance FromTable2 Tt where
    parseTable2 (Table2 ft) = case cast ft of
        Just t -> Tt
                    <$> t .:: (T.pack "Num")
                    <*> t .:: "Name"
                    <*> t .:: "Address"
                    <*> t .:: "Phone"
        Nothing -> error "Key type mismatch."
instance FromTable2 Int where
    parseTable2 (Cell2 t) = either (\e -> Left $ "Error in cell " ++ T.unpack t ++ ": " ++ e) Right $ A.parseOnly cellDecimal t
    parseTable2 _        = error "Type mismatch cell."
instance FromTable2 T.Text where
    parseTable2 (Cell2 t) = A.parseOnly (A.takeWhile isSpace *> A.takeText) t
    parseTable2 _        = error "Type mismatch at cell."

(.::) :: (FromTable2 a, TableKKey k) => M.Map k Table2 -> k -> Either String a
m .:: k             = lookupP2 parseTable2 m k

lookupP2 :: (FromTable2 a, TableKKey k) => (Table2 -> Either String a) -> M.Map k Table2 -> k -> Either String a
lookupP2 p o k      = case M.lookup k o of
    Just x  -> p x
    Nothing -> error $ "No such key: '" ++ show k ++ "'."

tableT2 :: A.Parser T.Text -> A.Parser Table2
tableT2 hp  = let t = tableNK2 hp
              in  Table2 . M.map (\x -> Table2 (M.map Cell2 x)) <$> t

testT2 :: IO (Either String [Tt])
testT2       = do
    c <- T.readFile "1.txt"
    case A.parseOnly (tableT2 trimWhitespace) c of
      Right tx -> return (parseTable2 tx)
      Left  e  -> error $ "Can't parse table with: " ++ e


data Table3 k   = forall t. TableKKey t => Table3 (M.Map k (Table3 t))
                 | Cell3 T.Text

class FromTable3 a k where
    parseTable3 :: Table3 k -> Either String a

-- | Can't be defined.
{-instance forall a t. FromTable3 a t => FromTable3 [a] Int where
    parseTable3 (Table3 t)    = foldM (\z x -> parseTable3 (x :: Table3 t) >>= \r -> return (r : z)) [] t-}
instance FromTable3 Tt T.Text where
    parseTable3 (Table3 t) = Tt
                    <$> t .::: (T.pack "Num")
                    <*> t .::: "Name"
                    <*> t .::: "Address"
                    <*> t .::: "Phone"
instance FromTable3 Int t where
    parseTable3 (Cell3 t) = either (\e -> Left $ "Error in cell " ++ T.unpack t ++ ": " ++ e) Right $ A.parseOnly cellDecimal t
    parseTable3 _        = error "Type mismatch cell."
instance FromTable3 T.Text t where
    parseTable3 (Cell3 t) = A.parseOnly (A.takeWhile isSpace *> A.takeText) t
    parseTable3 _        = error "Type mismatch at cell."

(.:::) :: (FromTable3 a t, TableKKey k) => M.Map k (Table3 t) -> k -> Either String a
m .::: k             = lookupP3 parseTable3 m k

lookupP3 :: (FromTable3 a t, TableKKey k) => (Table3 t -> Either String a) -> M.Map k (Table3 t) -> k -> Either String a
lookupP3 p o k      = case M.lookup k o of
    Just x  -> p x
    Nothing -> error $ "No such key: '" ++ show k ++ "'."


class FromTable4 a k where
    data Table4 a k :: *
    parseTable4 :: Table4 a k -> Either String a

instance FromTable4 Int t where
    data Table4 Int t  = CellI3 T.Text
    parseTable4 (CellI3 t) = either (\e -> Left $ "Error in cell " ++ T.unpack t ++ ": " ++ e) Right $ A.parseOnly cellDecimal t
    parseTable4 _        = error "Type mismatch cell."
instance FromTable4 T.Text t where
    data Table4 T.Text t   = CellT3 T.Text
    parseTable4 (CellT3 t) = A.parseOnly (A.takeWhile isSpace *> A.takeText) t
    parseTable4 _        = error "Type mismatch at cell."

{-data Table31    = Cell31 T.Text
                | Table31 (M.Map k v)-}

data Table5 k v :: * where
    Table5 :: M.Map k v -> Table5 k v
    Cell5  :: T.Text -> Table5 () T.Text
deriving instance (Show k, Show v) => Show (Table5 k v)

class FromTable5 a k where
    type Value5 a k
    parseTable5 :: Table5 k (Value5 a k) -> Either String a

instance FromTable5 a T.Text => FromTable5 [a] Int where
    type Value5 [a] Int     = Table5 T.Text (Value5 a T.Text)
    parseTable5 (Table5 mt) = foldM (\z x -> parseTable5 x >>= \r -> return (r : z)) [] mt
instance FromTable5 Tt T.Text where
    type Value5 Tt T.Text  = Table5 () T.Text
    parseTable5 (Table5 t) = Tt
                            <$> t .:-: "Num"
                            <*> t .:-: "Name"
                            <*> t .:-: "Address"
                            <*> t .:-: "Phone"
instance FromTable5 Int () where
    type Value5 Int () = T.Text
    parseTable5 (Cell5 t) = either (\e -> Left $ "Error in cell " ++ T.unpack t ++ ": " ++ e) Right $ A.parseOnly cellDecimal t
    parseTable5 _        = error "Type mismatch cell."
instance FromTable5 T.Text () where
    type Value5 T.Text () = T.Text
    parseTable5 (Cell5 t) = A.parseOnly (A.takeWhile isSpace *> A.takeText) t
    parseTable5 _        = error "Type mismatch at cell."

(.:-:) :: (Ord k, Show k, FromTable5 a t, Show (Value5 a t), Show t) => M.Map k (Table5 t (Value5 a t)) -> k -> Either String a
m .:-: k              = lookupP5 parseTable5 m k

lookupP5 :: (Ord k, Show k, Show v) => (v -> Either String a) -> M.Map k v -> k -> Either String a
lookupP5 p o k     = case M.lookup k o of
    Just x  -> p x
    Nothing -> error $ "No such key1: '" ++ show k ++ "'." ++ "in map: " ++ show o

tableT5 :: A.Parser T.Text -> A.Parser (Table5 Int (Table5 T.Text (Table5 () T.Text)))
tableT5 hp  = let t = tableNK2 hp
              in  Table5 . M.map (\x -> Table5 (M.map Cell5 x)) <$> t

row1T5 :: A.Parser T.Text -> A.Parser (Table5 T.Text (Table5 () T.Text))
row1T5 hp   = let t = tableNK2 hp
              in  Table5 . M.map Cell5 . snd . head . M.toList <$> t

row0T5 :: A.Parser T.Text -> A.Parser (Table5 Int (Table5 () T.Text))
row0T5 hp   = let t = tableNK2 hp
              --in  snd . head . M.toList . M.map (Cell5 . snd . head . M.toList) <$> t
              in  Table5 . M.map (Cell5 . snd . head . M.toList) <$> t

table2T5 :: A.Parser T.Text -> A.Parser (Table5 Int (Table5 Int (Table5 T.Text (Table5 () T.Text))))
table2T5 hp = let t = tableNK2 hp
              in  Table5 . M.singleton 1 .
                    Table5 . M.map (\x -> Table5 (M.map Cell5 x)) <$> t

testT5 :: IO (Either String [Tt])
testT5      = do
    c <- T.readFile "1.txt"
    case A.parseOnly (tableT5 trimWhitespace) c of
      Right tx -> return (parseTable5 tx)
      Left  e  -> error $ "Can't parse table with: " ++ e

-- | Does not compile due to type-mismatch. Compare with 'testRow1T', which
-- compiles, but fails at runtime.
{-testRow1T5 :: IO (Either String [Tt])
testRow1T5  = do
    c <- T.readFile "1.txt"
    case A.parseOnly (row1T5 trimWhitespace) c of
      Right tx -> return (parseTable5 tx)
      Left  e  -> error $ "Can't parse table with: " ++ e-}

-- | According to my instances table inside a list '[a]' must have 'T.Text'
-- keys.. So this also does not compile.
{-testRow0T5 :: IO (Either String [T.Text])
testRow0T5  = do
    c <- T.readFile "1.txt"
    case A.parseOnly (row0T5 trimWhitespace) c of
      Right tx -> return (parseTable5 tx)
      Left  e  -> error $ "Can't parse table with: " ++ e-}

-- | Does not compile due to type-mismatch. Compare with 'test2T', which
-- compiles, but fails at runtime.
{-test2T5 :: IO (Either String [[Tt]])
test2T5     = do
    c <- T.readFile "1.txt"
    case A.parseOnly (table2T5 trimWhitespace) c of
      Right tx -> return (parseTable5 tx)
      Left  e  -> error $ "Can't parse table with: " ++ e-}

class FromTable51 a k where
    type Value51 a :: * -> *
    parseTable51 :: Table5 k (Value5 a k) -> Either String a

{-instance FromTable51 a t => FromTable51 [a] Int where
    type Value51 [a] t       = Table5 t (Value51 a t)
    parseTable51 (Table5 mt) = foldM (\z x -> parseTable51 x >>= \r -> return (r : z)) [] mt-}

{-instance FromTableK31 T.Text t where
    data TableK31 T.Text t  = CellT31 T.Text
    parseTableK31 (CellT3 t) = A.parseOnly (A.takeWhile isSpace *> A.takeText) t
    parseTableK31 _         = error "Type mismatch at cell."-}

data TableK5 :: * -> * where
    TableText5 :: M.Map T.Text (TableK5 t) -> TableK5 T.Text

someFunc :: IO ()
someFunc = putStrLn "someFunc"

