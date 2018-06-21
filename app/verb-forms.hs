{-# LANGUAGE OverloadedStrings      #-}

module Main where

import           Data.Monoid
import qualified Data.Map               as M
import           Data.Yaml
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Options.Applicative    as A
import           Control.Monad

import Data.Aeson.Encode.Pretty
import qualified Sgf.Data.Text.Table    as T

import Sgf.Jp
import Sgf.Jp.Types
import Sgf.Jp.Types.VForms
import Sgf.Jp.VForms

putStrUtf8 :: T.Text -> IO ()
putStrUtf8 = BS.putStr . T.encodeUtf8 . (`T.append` "\n")

main :: IO ()
main = join . A.customExecParser (A.prefs A.showHelpOnError) $
    A.info (A.helper <*> parser)
    (  A.fullDesc
    <> A.header "Generate verb forms."
    <> A.progDesc "Generate files with verb forms according to definitions in config file."
    )
  where
    parser :: A.Parser (IO ())
    parser =
        work_
          <$> A.strOption
              (  A.long "config"
              <> A.short 'c'
              <> A.metavar "FILE"
              <> A.help "path to config file"
              <> A.value "verb-forms.yaml"
              <> A.showDefault
              )

work_ :: FilePath -> IO ()
work_ cf = do
    mconj <- T.decodeFileL "../conjugations.txt" >>= either
        (\e -> error $ "Can't parse JConj table " ++ e)
        (return . buildMap conjNumber)
    checkMap mconj

    t <- decodeFileEither cf
    tv <- case t of
      Right tv  -> BL.putStr (encodePretty (tv :: Value)) >> return tv
      Left e    -> putStrLn (prettyPrintParseException e) >> error "Huh.."
    let cf = either error id (parseEither parseJSON tv)
    mapM_ (writeRunSpec mconj) (cf :: [RunSpec])

