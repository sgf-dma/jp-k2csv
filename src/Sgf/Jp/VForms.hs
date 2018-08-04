{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module Sgf.Jp.VForms
    ( generateForms
    , writeRunSpec
    )
  where

import Data.Maybe
import qualified Data.List              as L
import           Data.List.Extra (snoc)
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Random.Shuffle
import           System.FilePath
import           System.Directory
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Arrow

import Sgf.Jp.Types
import Sgf.Jp.Types.VForms


writingToLine :: [T.Text] -> T.Text
writingToLine = T.concat . L.intersperse ", "

genSpec' :: VFormSpec -> StateT VFReader Maybe VForm2
genSpec' VFormSpec{..} = gets curJConj >>= go
  where
    go :: JConj -> StateT VFReader Maybe VForm2
    go jc
      | all (flip inConjTags jc) vformFilter = gets (rowMod . jconjMap) >>= \f ->
            lift (stem <$> f jc)
      | otherwise = mzero

-- | 'groupBy' version using second element of a pair ('State') to group list
-- elements.
groupByState :: (b -> b -> Bool) -> [(a, b)] -> [([a], b)]
groupByState _  []        = []
groupByState eq (x0 : xs) = foldr go (\(y, s) -> [([y], s)]) xs x0
  where
    -- | Check /next/ element state against /current/ element state and add
    -- /next/ element into corresponding result group.
    --go :: (a, b) -> ((a, b) -> [([a], b)]) -> (a, b) -> [([a], b)]
    go w@(_, s) zf (xN, sN)
      | s `eq` sN     = case zf w of
                        []              -> error "Impossible condition in `groupByState`."
                        ((ys, _) : zs)  -> (xN : ys, sN) : zs
      | otherwise   = ([xN], sN) : zf w

genLine :: LineSpec -> (VForm2 -> Writing) -> ReaderT VFReader Maybe T.Text
genLine (LineSpec vsp) f    = ReaderT $
    buildLine . map (second curJConj)
    . groupByState (\v1 v2 -> curJConj v1 == curJConj v2)
    . runStateT (lift vsp >>= go)
  where
    -- | Generate text according to 'VFormSpec'-s.
    go :: VFormSpec -> StateT VFReader [] T.Text
    go vs = do
      vf <- mapStateT maybeToList (genSpec' vs)
      let vt = writingToLine . f $ vf
      if T.null vt then mzero else pure vt
    -- | Build a line from several 'Writing'-s of a /single/ 'JConj'.
    buildLineS :: ([T.Text], JConj) -> T.Text
    buildLineS (ts, jc) = T.concat . L.intersperse "; "
                            . flip snoc (T.pack . show $ conjNumber jc)
                            $ ts
    -- | Build a line from several blocks for /different/ 'JConj'-s.
    buildLine :: [([T.Text], JConj)] -> Maybe T.Text
    buildLine []    = mzero
    buildLine xs    = pure . T.concat . L.intersperse ". " . map buildLineS $ xs

genLine' :: [LineSpec] -> (VForm2 -> Writing) -> ReaderT VFReader [] T.Text
genLine' lsp f = lift lsp >>= mapReaderT maybeToList . flip genLine f

zipM :: Monad m => m [a] -> m [b] -> m [(a, b)]
zipM mxs mys    = do
    xs <- mxs
    ys <- mys
    return (zip xs ys)

generateForms' :: QSpec -> VFReader -> [(T.Text, T.Text)]
generateForms' QSpec{..} = zipM (runReaderT questions) (runReaderT answer)
  where
    questions :: ReaderT VFReader [] T.Text
    questions   = asks (questionWriting . curJConj) >>= genLine' questionSpec
    -- There is only answer, i just repeat to match the number of questions.
    answer :: ReaderT VFReader [] T.Text
    answer      = mapReaderT (maybe [] repeat) (asks (answerWriting . curJConj) >>= genLine answerSpec)

--generateForms :: Foldable t => [QSpec] -> t [JConj] -> [(T.Text, T.Text)]
generateForms :: [QSpec] -> M.Map Int [JConj] -> [(T.Text, T.Text)]
generateForms rs jcs = foldr ((++) . go) [] jcs
  where
    vfr :: JConj -> VFReader
    vfr jc = VFReader {curJConj = jc, jconjMap = jcs}
    go :: [JConj] -> [(T.Text, T.Text)]
    go ys = rs >>= \r -> ys >>= generateForms' r . vfr

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
writeRunSpec mconj rs@RunSpec{..} = do
    print runFilter
    print rs
    writeVerbFiles files (T.unpack runName) . unzip
        . generateForms runSpec
        . M.filter (applyFilter runFilter)
        $ mconj
  where
    applyFilter :: Maybe LNumFilter -> [JConj] -> Bool
    applyFilter Nothing     = const True
    applyFilter (Just p)    = any (inConjLnums (lnumFilter p))

