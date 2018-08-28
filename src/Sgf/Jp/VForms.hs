{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module Sgf.Jp.VForms
    ( generateForms
    , writeRunSpec
    )
  where

import qualified Data.List              as L
import           Data.List.Extra (snoc)
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Random.Shuffle
import           System.FilePath
import           System.Directory
import           Control.Monad

import Sgf.Jp.Types
import Sgf.Jp.Types.VForms


writingToLine :: [T.Text] -> T.Text
writingToLine = T.concat . L.intersperse ", "

genSpec' :: VFormSpec -> JConj -> VForm2
genSpec' VFormSpec {..} x =
    let v@VForm2 {..} = stem x
    in  v
            { kanaForm2 =
                if null kanaForm2 then []
                    else kanaForm2
            , kanjiForm2 =
                if null kanjiForm2 then []
                    else kanjiForm2
            }

genLine :: LineSpec -> (VForm2 -> Writing) -> JConj -> T.Text
genLine (LineSpec vsp) f    = do
    vs <- mapM genSpec' vsp
    jn <- conjNumber
    return . T.concat . L.intersperse "; "
        . filter (not . T.null)
        . flip snoc (T.pack . show $ jn) . map (writingToLine . f)
        $ vs

genLine' :: [LineSpec] -> (VForm2 -> Writing) -> JConj -> [T.Text]
genLine' lsp f x  = map (\l -> genLine l f x) lsp

zipM :: Monad m => m [a] -> m [b] -> m [(a, b)]
zipM mxs mys    = do
    xs <- mxs
    ys <- mys
    return (zip xs ys)

generateForms' :: QSpec -> JConj -> [(T.Text, T.Text)]
generateForms' QSpec{..} = zipM questions (sequence (repeat answer))
  where
    questions :: JConj -> [T.Text]
    questions   = questionWriting >>= genLine' questionSpec
    answer :: JConj -> T.Text
    answer      = answerWriting >>= genLine answerSpec

generateForms :: Foldable t => [QSpec] -> t [JConj] -> [(T.Text, T.Text)]
generateForms rs    = foldr ((++) . go) []
  where
    go :: [JConj] -> [(T.Text, T.Text)]
    go ys = rs >>= \r -> ys >>= generateForms' r

lnumFilter :: LNumFilter -> LNum -> Bool
lnumFilter LessonRange{..} LNum{..}  =    maybe True (<= lessonNum) lnumFrom
                                  && maybe True (>= lessonNum) lnumTill
lnumFilter Lesson{..} LNum{..}  = lnumEq == lessonNum

writeVerbFiles :: FileSpec -> String -> ([T.Text], [T.Text]) -> IO ()
writeVerbFiles FileSpec{..} runName (conjFormsQ, conjFormsA) = do
    createDirectoryIfMissing True destDir
    T.writeFile qfn  (T.unlines conjFormsQ)
    T.writeFile afn (T.unlines conjFormsA)
    forM_ (take nfiles [1..]) $ \n -> do
      (randFormsQ, randFormsA) <- fmap unzip . shuffleM $ zip conjFormsQ
                                                              conjFormsA
      T.writeFile (qrfn n) (T.unlines randFormsQ)
      T.writeFile (arfn n) (T.unlines randFormsA)
  where
    qfn    = destDir </> "vforms-" ++ runName ++ "-Q" ++ ".txt"
    qrfn n = destDir </> "random-" ++ runName ++ "-" ++ show n ++ "-Q" ++ ".txt"
    afn    = destDir </> "vforms-" ++ runName ++ "-A" ++ ".txt"
    arfn n = destDir </> "random-" ++ runName ++ "-" ++ show n ++ "-A" ++ ".txt"

writeRunSpec :: M.Map Int [JConj] -> RunSpec -> IO ()
writeRunSpec mconj RunSpec{..} = do
    print runFilter
    writeVerbFiles files (T.unpack runName) . unzip
        . generateForms runSpec
        . M.filter (applyFilter runFilter)
        $ mconj
  where
    applyFilter :: Maybe LNumFilter -> [JConj] -> Bool
    applyFilter Nothing     = const True
    applyFilter (Just p)    = any (inConjLnums (lnumFilter p))

