{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module Sgf.Jp.VForms
    ( generateForms
    , writeRunSpec
    )
  where

import Data.Maybe
import Data.Monoid
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


modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

genSpec :: VFormSpec -> JConj -> ReaderT VFReader Maybe VForm2
genSpec VFormSpec{..} jc = do
    VFReader{..} <- ask
    go (getLast (runFilter runSpec <> vformFilter)) curJConj
  where
    go :: Maybe VFormFilter -> JConj -> ReaderT VFReader Maybe VForm2
    go mvf jc | maybe True (flip applyFilter jc) mvf = lift (stem jc)
              | otherwise = mzero

genSpec' :: VFormSpec -> StateT VFReader Maybe VForm2
genSpec' VFormSpec{..} = do
    modifyM f
    VFReader{..} <- get
    go (getLast (runFilter runSpec <> vformFilter)) curJConj
  where
    f :: VFReader -> StateT VFReader Maybe VFReader
    f vf@VFReader{..} = do
        nj <- lift (rowMod jconjMap curJConj)
        pure (vf{curJConj = nj})
    go :: Maybe VFormFilter -> JConj -> StateT VFReader Maybe VForm2
    go mvf jc | maybe True (flip applyFilter jc) mvf = lift (stem jc)
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

generateForms' :: QSpec -> ReaderT VFReader [] (T.Text, T.Text)
generateForms' QSpec{..} = ReaderT $ zipM (runReaderT questions) (runReaderT answer)
  where
    questions :: ReaderT VFReader [] T.Text
    questions   = asks (questionWriting . curJConj) >>= genLine' questionSpec
    -- There is only answer, i just repeat to match the number of questions.
    answer :: ReaderT VFReader [] T.Text
    answer      = mapReaderT (maybe [] repeat) (asks (answerWriting . curJConj) >>= genLine answerSpec)

generateForms :: ReaderT VFReader [] (T.Text, T.Text)
generateForms = do
    VFReader{..} <- ask
    jc <- lift $ concat (M.elems jconjMap)
    q  <- lift (qsSpec runSpec)
    local (\vf -> vf{curJConj = jc}) (generateForms' q)

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
    let vfr = VFReader {jconjMap = mconj, runSpec = rs, curJConj = undefined}
    writeVerbFiles files (T.unpack runName) . unzip
        $ runReaderT generateForms vfr

lnumFilter :: LNumFilter -> LNum -> Bool
lnumFilter LessonRange{..} LNum{..}  =    maybe True (<= lessonNum) lnumFrom
                                  && maybe True (>= lessonNum) lnumTill
lnumFilter Lesson{..} LNum{..}  = lnumEq == lessonNum

applyFilter :: VFormFilter -> JConj -> Bool
applyFilter VFormFilter{..} =
    (&&)
        <$> applyLFilter lFilter
        <*> applyTagFilter tagFilter
  where
    applyLFilter :: Maybe LNumFilter -> JConj -> Bool
    applyLFilter Nothing    = const True
    applyLFilter (Just p)   = inConjLnums (lnumFilter p)
    applyTagFilter :: [T.Text] -> JConj -> Bool
    applyTagFilter ts jc = all (flip inConjTags jc) ts

