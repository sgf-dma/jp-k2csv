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

import Sgf.Jp.Types
import Sgf.Jp.Types.VForms


writingToLine :: [T.Text] -> T.Text
writingToLine = T.concat . L.intersperse ", "

genSpec' :: VFormSpec -> StateT JConj Maybe VForm2
genSpec' VFormSpec{..} = get >>= go
  where
    go :: JConj -> StateT JConj Maybe VForm2
    go jc
      | all (flip inConjTags jc) vformFilter = return (stem jc)
      | otherwise = mzero

-- | 'groupBy' version using second element of a pair ('State') to group list
-- elements.
groupByState :: Eq b => [(a, b)] -> [([a], b)]
groupByState []       = []
groupByState (x0 : xs) = foldr go (\(y, s) -> [([y], s)]) xs x0
  where
    -- | Check /next/ element state against /current/ element state and add
    -- /next/ element into corresponding result group.
    go :: Eq b => (a, b) -> ((a, b) -> [([a], b)]) -> (a, b) -> [([a], b)]
    go w@(_, s) zf (xN, sN)
      | s == sN     = case zf w of
                        []              -> error "Impossible condition in `groupByState`."
                        ((ys, _) : zs)  -> (xN : ys, sN) : zs
      | otherwise   = ([xN], sN) : zf w

genLine :: LineSpec -> (VForm2 -> Writing) -> ReaderT JConj Maybe T.Text
genLine (LineSpec vsp) f    = ReaderT $
    buildLine . groupByState . runStateT (lift vsp >>= go)
  where
    -- | Generate text according to 'VFormSpec'-s.
    go :: VFormSpec -> StateT JConj [] T.Text
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

genLine' :: [LineSpec] -> (VForm2 -> Writing) -> ReaderT JConj [] T.Text
genLine' lsp f = lift lsp >>= mapReaderT maybeToList . flip genLine f

zipM :: Monad m => m [a] -> m [b] -> m [(a, b)]
zipM mxs mys    = do
    xs <- mxs
    ys <- mys
    return (zip xs ys)

generateForms' :: QSpec -> JConj -> [(T.Text, T.Text)]
generateForms' QSpec{..} = zipM (runReaderT questions) (runReaderT answer)
  where
    questions :: ReaderT JConj [] T.Text
    questions   = asks questionWriting >>= genLine' questionSpec
    -- There is only answer, i just repeat to match the number of questions.
    answer :: ReaderT JConj [] T.Text
    answer      = mapReaderT (maybe [] repeat) (asks answerWriting >>= genLine answerSpec)

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

