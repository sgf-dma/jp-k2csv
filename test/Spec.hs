{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveDataTypeable     #-}

import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Attoparsec.Text   as A
import qualified Data.Map               as M

import Sgf.Data.Text.Table


data Tt          = Tt
                        { number        :: Int
                        , name      :: T.Text
                        , addr      :: T.Text
                        , phone      :: T.Text
                        }
  deriving (Show, Read)

instance FromTable Tt where
    parseTable      = withTableText "Tt" $ \m ->
                        Tt
                            <$> m .: "Num"
                            <*> m .: "Name"
                            <*> m .: "Address"
                            <*> m .: "Phone"

row1T :: A.Parser T.Text -> A.Parser Table
row1T hp    = let t = tableP hp
              in  TableText . M.map Cell . snd . head . M.toList <$> t

table2T :: A.Parser T.Text -> A.Parser Table
table2T hp  = let t = tableP hp
              in  TableInt . M.singleton 1 .
                    TableInt . M.map (\x -> TableText (M.map Cell x)) <$> t

testT :: IO (Either String [Tt])
testT       = do
    c <- T.readFile "test/table.txt"
    case A.parseOnly (tableT trimWhitespace) c of
      Right tx -> return (parseTable tx)
      Left  e  -> error $ "Can't parse table with: " ++ e

-- | Does not work due to mismatch of table structure expected from result
-- type and actual. Will work, if result type will be 'Either String Tt'.
testRow1T :: IO (Either String [Tt])
testRow1T   = do
    c <- T.readFile "test/table.txt"
    case A.parseOnly (row1T trimWhitespace) c of
      Right tx -> return (parseTable tx)
      Left  e  -> error $ "Can't parse table with: " ++ e

-- | Does not work due to mismatch of table structure expected from result
-- type and actual. Will work, if result type will be 'Either String [[Tt]]'.
test2T :: IO (Either String [Tt])
test2T      = do
    c <- T.readFile "test/table.txt"
    case A.parseOnly (table2T trimWhitespace) c of
      Right tx -> return (parseTable tx)
      Left  e  -> error $ "Can't parse table with: " ++ e

main :: IO ()
main                = do
    Right tts <- testT
    print tts

