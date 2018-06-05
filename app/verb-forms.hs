{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TupleSections          #-}

module Main where

import qualified Data.Map               as M
import           Data.Yaml
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL

import Data.Aeson.Encode.Pretty
import qualified Sgf.Data.Text.Table    as T

import Sgf.Jp
import Sgf.Jp.Types
import Sgf.Jp.Types.VForms
import Sgf.Jp.VForms

putStrUtf8 :: T.Text -> IO ()
putStrUtf8 = BS.putStr . T.encodeUtf8 . (`T.append` "\n")

main :: IO ()
main = do
    mconj <- T.decodeFileL "../conjugations.txt" >>= either
        (\e -> error $ "Can't parse JConj table " ++ e)
        (return . buildMap conjNumber)
    checkMap mconj

    t <- decodeFileEither "verb-forms.yaml"
    tv <- case t of
      Right tv  -> BL.putStr (encodePretty (tv :: Value)) >> return tv
      Left e    -> putStrLn (prettyPrintParseException e) >> error "Huh.."
    let cf = either (\e -> error e) id (parseEither parseJSON tv)
    mapM_ (writeRunSpec mconj) (cf :: [RunSpec])

