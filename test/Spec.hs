{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveDataTypeable     #-}

import qualified Data.Text              as T

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

testT :: IO (Either String [Tt])
testT       = decodeFile "test/table.txt"

main :: IO ()
main                = do
    Right tts <- testT
    print tts

