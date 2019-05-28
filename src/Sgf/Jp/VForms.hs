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
import qualified TextShow           as T
import           System.Random.Shuffle
import           System.FilePath
import           System.Directory
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Arrow

import Sgf.Jp.Types
import Sgf.Jp.Types.VForms

import qualified Data.Text.Encoding     as T
import qualified Data.ByteString        as BS

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

genSpec :: VFormSpec -> JConj -> Maybe VForm2
genSpec VFormSpec{..} = go (getLast vformFilter)
  where
    go :: Maybe VFormFilter -> JConj -> Maybe VForm2
    go mvf jc | maybe True (flip applyFilter jc) mvf = stem jc
              | otherwise = mzero

genSpec' :: VFormSpec -> StateT VFReader Maybe VForm2
genSpec' VFormSpec{..} = do
    modifyM f
    VFReader{..} <- get
    -- FIXME: Why i combine filters? Run filter should be _already_ applied.
    -- Now i should apply vform filter only, shouldn't i?
    go (getLast vformFilter) curJConj
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

-- FIXME: Remove tuple from here and just add `JConj` into `VForm2`.
genSpec2 :: VFormSpec -> (VForm2 -> Writing) -> M.Map Int [JConj] -> JConj -> [(T.Text, JConj)]
genSpec2 vfs@VFormSpec{..} f jcm jc = do
    jc' <- maybeToList (rowMod jcm jc)
    maybe mzero (\t -> return (t, jc')) $ genSpec vfs jc' >>= go
  where
    -- FIXME: Merge this function into `writingToLine`.
    go :: VForm2 -> Maybe T.Text
    go vf   = let vt = writingToLine (f vf)
              in  if T.null vt then mzero else pure vt

-- FIXME: Grouping is completely broken..
g2 :: LineSpec -> (VForm2 -> Writing) -> ReaderT VFReader Maybe T.Text
g2 (LineSpec vsp) f = do
    VFReader{..} <- ask
    let h = buildLine $ concatMap (\vf -> groupByState (==) $ genSpec2 vf f jconjMap curJConj) vsp
        h2 = buildLine $ groupByState (==) $ concatMap (\vf -> genSpec2 vf f jconjMap curJConj) vsp
        h3 = buildLine . groupByState (\x y -> conjNumber x == conjNumber y)
                $ concatMap (\vf -> genSpec2 vf f jconjMap curJConj) vsp
    lift h3
  where
    -- | Build a line from several 'Writing'-s of a /single/ 'JConj'.
    buildLineS :: ([T.Text], JConj) -> T.Text
    buildLineS (ts, jc) = T.concat . L.intersperse "; "
                            . flip snoc (T.pack . show $ conjNumber jc)
                            $ ts
    -- | Build a line from several blocks for /different/ 'JConj'-s.
    buildLine :: [([T.Text], JConj)] -> Maybe T.Text
    buildLine []    = mzero
    buildLine xs    = pure . T.concat . L.intersperse ". " . map buildLineS $ xs

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

g2' :: [LineSpec] -> (VForm2 -> Writing) -> ReaderT VFReader [] T.Text
g2' lsp f = lift lsp >>= mapReaderT maybeToList . flip g2 f

--zipM :: Monad m => m [a] -> m [b] -> m [(a, b)]
zipM mxs mys    = do
    xs <- mxs
    when (xs == [] || xs == [T.empty]) $ error "Huy"
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

generateForms'2 :: QSpec -> ReaderT VFReader [] (T.Text, T.Text)
generateForms'2 QSpec{..} = ReaderT $ zipM (runReaderT questions) (runReaderT answer)
  where
    questions :: ReaderT VFReader [] T.Text
    questions   = asks (questionWriting . curJConj) >>= g2' questionSpec
    -- There is only answer, i just repeat to match the number of questions.
    answer :: ReaderT VFReader [] T.Text
    answer      = mapReaderT (maybe [] repeat) (asks (answerWriting . curJConj) >>= g2 answerSpec)

generateForms :: ReaderT VFReader [] (T.Text, T.Text)
generateForms = do
    VFReader{..} <- ask
    -- FIXME: Remove Last monoid!
    let p = maybe (const True) applyFilter (getLast (runFilter runSpec))
    jc <- lift . filter p $ concat (M.elems jconjMap)
    q  <- lift (qsSpec runSpec)
    local (\vf -> vf{curJConj = jc}) (generateForms' q)

generateForms2 :: ReaderT VFReader [] (T.Text, T.Text)
generateForms2 = do
    VFReader{..} <- ask
    -- FIXME: Remove Last monoid!
    let p = maybe (const True) applyFilter (getLast (runFilter runSpec))
    jc <- lift . filter p $ concat (M.elems jconjMap)
    q  <- lift (qsSpec runSpec)
    local (\vf -> vf{curJConj = jc}) (generateForms'2 q)

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

putStrUtf8 :: T.Text -> IO ()
putStrUtf8 = BS.putStr . T.encodeUtf8 . (`T.append` "\n")

debugLS :: VFReader -> IO ()
debugLS vfr@VFReader{..} = do
    let qw = isKanji False curJConj
        aw = isKanji True curJConj
    print "Question:"
    forM_ (concatMap questionSpec (qsSpec runSpec)) $ \ls -> do
        let t = fromMaybe "huy" $ runReaderT (g2 ls qw) vfr
        putStrUtf8 t
        let LineSpec vs = ls
        forM vs $ \v -> do
            let ys = genSpec2 v qw jconjMap curJConj
                bs = map (first T.encodeUtf8) ys
                ys' = map (second conjNumber) ys
            --BS.putStrLn bs
            putStrUtf8 (T.showt ys')
    print "Answer:"
    forM_ (map answerSpec (qsSpec runSpec)) $ \ls -> do
        let t = fromMaybe "huy" $ runReaderT (g2 ls qw) vfr
        putStrUtf8 t
        let LineSpec vsp = ls
        forM vsp $ \v -> do
            let ys = genSpec2 v qw jconjMap curJConj
                bs = map (first T.encodeUtf8) ys
                ys' = map (second conjNumber) ys
            --BS.putStrLn bs
            putStrUtf8 (T.showt ys')
        let h0 = concatMap (\vf -> genSpec2 vf aw jconjMap curJConj) vsp
            h0' = map (second conjNumber) h0
            h2 = groupByState (\x y -> conjNumber x == conjNumber y) $ h0
            h2' = map (second conjNumber) h2
        putStrUtf8 (T.showt h0')
        putStrUtf8 (T.showt h2')

writeRunSpec :: M.Map Int [JConj] -> RunSpec -> IO ()
writeRunSpec mconj rs@RunSpec{..} = do
    print runFilter
    print rs
    print (M.size mconj)
    print "Debug run"
    let vfr = VFReader {jconjMap = mconj, runSpec = rs, curJConj = undefined}
        r1 = runReaderT generateForms vfr
        r2 = runReaderT generateForms2 vfr
    let p = maybe (const True) applyFilter (getLast runFilter)
    mapM_ (\jc -> debugLS (vfr{curJConj = jc})) (filter p $ concat (M.elems mconj))
    when (r1 /= r2) $ error "v1 and v2 mismatch!"
    writeVerbFiles files (T.unpack runName <> "-v2") . unzip $ r2
    writeVerbFiles files (T.unpack runName <> "-v1") . unzip $ r1

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

