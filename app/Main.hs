{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}

module Main where

import Data.Char
import qualified Data.String as S
import qualified Data.List as L
import qualified Data.Attoparsec.Text        as A
import qualified Data.Text.Encoding as T
import qualified Data.ByteString    as BS
import Data.List.Split
import Data.Csv
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map as M
import Control.Arrow (first)
import Control.Monad.State
import Control.Applicative
import Turtle.Shell
import Turtle.Line
import Data.List.Extra (snoc)
import Data.Maybe

import qualified Sgf.Data.Text.Table    as T
import Sgf.Data.Text.Table.Parse
import Sgf.JPWords.Checks

import System.Random.Shuffle

-- | One step in recursively string parsing: if there is no more input left,
-- finish with current result. Otherwise, try to read on more value. Here
-- recursive call returns to caller, so i may add current step's result then
-- to preserve input element order.
step :: Serialize a => ([a] -> ReadS [a])   -- ^ Recurse call.
        -> [a]                              -- ^ Current result.
        -> ReadS [a]
step _ zs []        = [(zs, [])]
step rec zs s       = case fromString s of
    [] -> rec zs []
    xs -> concatMap (\(x, r) -> fmap (first (x :)) (rec zs r)) xs

parseAll :: Serialize a => ReadS [a]
parseAll            = fix step []

-- | Read one line and split by \" s \", where s is some character.
readLine :: Char -> ReadS [String]
readLine s          = go . span (/= '\n')
      where
        sep :: String
        sep         = [' ', s, ' ']
        go :: (String, String) -> [([String], String)]
        go ([],    [])  = []
        go ([], _ : r)  = readLine s r  -- Empty string.
        go (l , r)      =
            let xs = splitOn sep l
                ys = map (L.dropWhileEnd isSpace . dropWhile isSpace) xs
            in  [(ys, drop 1 r)]

type ReadM a        = StateT String [] a
toReadM :: ReadS a -> ReadM a
toReadM             = StateT

class Serialize a where
    fromString :: ReadS a
    toString :: a -> ShowS

data JWord          = JWord
                        { number        :: Int
                        , reference     :: String
                        , reading1      :: String
                        , reading2      :: String
                        , origin        :: String
                        , translate     :: String
                        , description   :: String
                        , seeAlso       :: String
                        , tags          :: String
                        }
  deriving (Show, Read)
defJWord :: JWord
defJWord            = JWord
                        { number        = 0
                        , reference     = ""
                        , reading1      = ""
                        , reading2      = ""
                        , origin        = ""
                        , translate     = ""
                        , description   = ""
                        , seeAlso       = ""
                        , tags          = ""
                        }

instance T.FromTable JWord where
    parseTable      = T.withTableText "JWord" $ \m ->
        JWord
            <$> m T..: "Num"
            <*> (T.unpack <$> m T..: "Reference")
            <*> (T.unpack <$> m T..: "Reading1")
            <*> (T.unpack <$> m T..: "Reading2")
            <*> (T.unpack <$> m T..: "Origin")
            <*> (T.unpack <$> m T..: "Translation")
            <*> (T.unpack <$> m T..: "Description")
            <*> (T.unpack <$> m T..: "SeeAlso")
            <*> (T.unpack <$> m T..: "Tags")

instance Serialize JWord where
    fromString k    = flip runStateT k $ do
        [n, ref, r1, r2, on, tr, ds, sa, ts] <- toReadM (readLine ':')
        return JWord
            { number        = read n
            , reference     = ref
            , reading1      = r1
            , reading2      = r2
            , origin        = on
            , translate     = tr
            , description   = ds
            , seeAlso       = sa
            , tags          = ts
            }

instance ToRecord JWord where
    toRecord JWord {..} = record
                            [ toField number
                            , toField reference
                            , toField reading1
                            , toField reading2
                            , toField origin
                            , toField translate
                            , toField description
                            , toField seeAlso
                            , toField tags
                            ]

data JConj          = JConj
                        { conjNumber    :: Int
                        , conjReference :: String
                        , dictForm      :: String
                        , dictFormK     :: String
                        , masuForm      :: String
                        , masuFormK     :: String
                        , teForm        :: String
                        , teFormK       :: String
                        , naiForm       :: String
                        , naiFormK      :: String
                        , conjTags      :: String
                        }
  deriving (Show, Read)
defJConj :: JConj
defJConj            = JConj
                        { conjNumber    = 0
                        , conjReference = ""
                        , dictForm      = ""
                        , dictFormK     = ""
                        , masuForm      = ""
                        , masuFormK     = ""
                        , teForm        = ""
                        , teFormK       = ""
                        , naiForm       = ""
                        , naiFormK      = ""
                        , conjTags      = ""
                        }
instance T.FromTable JConj where
    parseTable      = T.withTableText "JConj" $ \m ->
        JConj
            <$> m T..: "Num"
            <*> (T.unpack <$> m T..: "Reference")
            <*> (T.unpack <$> m T..: "Dict form")
            <*> (T.unpack <$> m T..: "Dict kanji")
            <*> (T.unpack <$> m T..: "ます-form")
            <*> (T.unpack <$> m T..: "ます kanji")
            <*> (T.unpack <$> m T..: "て-form")
            <*> (T.unpack <$> m T..: "て kanji")
            <*> (T.unpack <$> m T..: "ない-form")
            <*> (T.unpack <$> m T..: "ない kanji")
            <*> (T.unpack <$> m T..: "Tags")

instance ToRecord JConj where
    toRecord JConj {..} = record
                            [ toField conjNumber
                            , toField conjReference
                            , toField dictForm
                            , toField dictFormK
                            , toField masuForm
                            , toField masuFormK
                            , toField teForm
                            , toField teFormK
                            , toField naiForm
                            , toField naiFormK
                            , toField conjTags
                            ]


data JKana          = JKana
                        { hiragana  :: String
                        , katakana  :: String
                        , syllable  :: String
                        , desc      :: String
                        }
  deriving (Show, Read)
defJKana :: JKana
defJKana            = JKana
                        { hiragana  = ""
                        , katakana  = ""
                        , syllable  = ""
                        , desc      = ""
                        }


instance Serialize JKana where
    fromString k    = flip runStateT k $ do
        [hn, kn, sy, dc] <- toReadM (readLine ':')
        return JKana
            { hiragana  = hn
            , katakana  = kn
            , syllable  = sy
            , desc      = dc
            }

instance ToRecord JKana where
    toRecord JKana {..} = record
                            [ toField hiragana
                            , toField katakana
                            , toField syllable
                            , toField desc
                            ]

buildMap :: (a -> Int) -> [a] -> M.Map Int [a]
buildMap toKey      = foldr (\x -> M.insertWith (++) (toKey x) [x]) M.empty

writeMap :: ToRecord a => FilePath -> M.Map Int [a] -> IO ()
writeMap f          = BL.writeFile f . encode . concat . M.elems

checkMap :: M.Map Int [a] -> IO ()
checkMap m          = do
    let ds = M.filter ((> 1) . length) m
    print $ "Duplicate numbers: " ++ show (M.keys ds)
    print $ "Max number: " ++ show (fst $ M.findMax m)

-- | Possible foreign words. Though, filtering may be greatly improved..
possibleForeign :: M.Map Int [JWord] -> M.Map Int [JWord]
possibleForeign     = M.map (filter (all isAscii . origin))

checkRefs :: M.Map Int [JWord] -> IO ()
checkRefs           = view . bothKanjiRefAndRel . onlyRefs
  where
    onlyRefs :: M.Map Int [JWord] -> Shell Line
    onlyRefs        = select . textToLines . S.fromString . reference
                        <=< select <=< select

inConjTags :: T.Text -> JConj -> Bool
inConjTags t        = (t `elem`) . toWords . T.pack . conjTags

toWords :: T.Text -> [T.Text]
toWords         = either (const []) id . A.parseOnly
    ( some $    A.takeWhile1 (not . A.isHorizontalSpace)
             <* A.takeWhile isSpace )

-- Take input till separator parser succeeds. Predicate is used to identify
-- character at which to try to match separator parser. Separator is dropped
-- from result. Chunks not ending on separator are _not_ included.
takeTillSep :: (Char -> Bool) -> A.Parser T.Text -> A.Parser T.Text
takeTillSep p sepP  =
    (fmap T.concat . many $ T.append
            <$> whenNotP sepP (T.singleton <$> A.anyChar)
            <*> A.takeWhile p)
        <* sepP

-- Take input till string parser succeeds. Predicate is used to identify
-- character at which to try to match string parser. Matched string is
-- _included_ in result (it will be at the end of result text). Chunks not
-- ending on matched string are _not_ included.
takeTillStr :: (Char -> Bool) -> A.Parser T.Text -> A.Parser T.Text
takeTillStr p strP  =
     fmap T.concat . snoc
        <$> (many $ T.append
                <$> whenNotP strP (T.singleton <$> A.anyChar)
                <*> A.takeWhile p)
        <*> strP

singleWordSuf :: T.Text -> A.Parser T.Text -> T.Text -> [T.Text]
singleWordSuf suf sepP = either (const []) id
    . A.parseOnly (some (takeTillSep (/= T.head suf) sepP) <* A.endOfInput)

-- Generate several words with different suffixes in place of original.
replaceSuffix :: T.Text -> [T.Text] -> T.Text -> [T.Text]
replaceSuffix sf rs t = do
    r <- rs
    w <- singleWordSuf sf
            (   A.string (sf `T.append` ", ")
            <|> A.string sf <* A.endOfInput)
            t
    return (w `T.append` r)

prependConjNum :: T.Text -> JConj -> T.Text
prependConjNum t    = let f = flip T.append
                      in  f t . f ", " . T.pack . show . conjNumber

appendConjNum :: T.Text -> JConj -> T.Text
appendConjNum t     = T.append t . T.append ", " . T.pack . show . conjNumber

genDictForms :: Bool -> JConj -> [T.Text]
genDictForms isKanji = gen . dictStem >>= mapM appendConjNum
  where
    dictStem :: JConj -> T.Text
    dictStem x
      | not isKanji || null (dictFormK x)   = T.pack (dictForm x)
      | otherwise                           = T.pack (dictFormK x)
    gen :: T.Text -> [T.Text]
    gen x           = map (x `T.append`)
                        [ "前に"
                        , "ことができます"
                        ]

genMasuForms :: Bool -> JConj -> [T.Text]
genMasuForms isKanji x = let t = masuStem x
                         in  mapM appendConjNum (t : gen t) x
  where
    masuStem :: JConj -> T.Text
    masuStem x
      | not isKanji || null (masuFormK x)   = T.pack (masuForm x)
      | otherwise                           = T.pack (masuFormK x)
    gen :: T.Text -> [T.Text]
    gen             = "ます" `replaceSuffix`
                        [ "たい"
                        , "たくない"
                        , "たかった"
                        , "たくなかった"
                        ]

genTeForms :: Bool -> JConj -> [T.Text]
genTeForms isKanji  = (gen <++> gen') . teStem >>= mapM appendConjNum
  where
    teStem :: JConj -> T.Text
    teStem x
      | not isKanji || null (teFormK x) = T.pack (teForm x)
      | otherwise                       = T.pack (teFormK x)
    gen :: T.Text -> [T.Text]
    gen             = "て" `replaceSuffix`
                        [ "てください"
                        , "てもいいです"
                        , "てはいけません"
                        , "ています"
                        ]
    gen' :: T.Text -> [T.Text]
    gen'            = "で" `replaceSuffix`
                        [ "でください"
                        , "でもいいです"
                        , "ではいけません"
                        , "でいます"
                        ]

genTaForms :: Bool -> JConj -> [T.Text]
genTaForms isKanji  = (gen <++> gen') . taStem >>= mapM appendConjNum
  where
    taStem :: JConj -> T.Text
    taStem x
      | not isKanji || null (teFormK x) = T.pack (teForm x)
      | otherwise                       = T.pack (teFormK x)
    gen :: T.Text -> [T.Text]
    gen             = "て" `replaceSuffix`
                        [ "たことがあります"
                        , "たり"
                        ]
    gen' :: T.Text -> [T.Text]
    gen'            = "で" `replaceSuffix`
                        [ "だことがあります"
                        , "だり"
                        ]

genNaiForms :: Bool -> JConj -> [T.Text]
genNaiForms isKanji = gen . naiStem >>= mapM appendConjNum
  where
    naiStem :: JConj -> T.Text
    naiStem x
      | not isKanji || null (naiFormK x)    = T.pack (naiForm x)
      | otherwise                           = T.pack (naiFormK x)
    gen :: T.Text -> [T.Text]
    gen             = "ない" `replaceSuffix`
                        [ "ないでください"
                        , "なくてもいいです"
                        , "なければなりません"
                        ]

infixl 3 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>)              = liftA2 (++)

generateForms :: Bool -> M.Map Int [JConj] -> [T.Text]
generateForms isKanjiAlways = M.fold go []
  where
    isKanji :: JConj -> Bool
    isKanji         = (isKanjiAlways ||) <$> inConjTags "kanji"
    go :: [JConj] -> [T.Text] -> [T.Text]
    go x zs = concatMap
                    (    flip genDictForms  <*> isKanji
                    <++> flip genMasuForms  <*> isKanji
                    <++> flip genTeForms    <*> isKanji
                    <++> flip genTaForms    <*> isKanji
                    <++> flip genNaiForms   <*> isKanji
                    )

                    {-(   masuForms <++> teForms <++> genTaForms False
                    <++> naiForms <++> genDictForms False )-}
                    x
                ++ zs

putStrUtf8 :: T.Text -> IO ()
putStrUtf8          = BS.putStr . T.encodeUtf8 . (`T.append` "\n")

randomWrite :: FilePath -> [T.Text] -> IO ()
randomWrite fn xs   = shuffleM xs >>= T.writeFile fn . T.unlines

main :: IO ()
main = do
    m <-  T.decodeFileL "../words-mnn.txt" >>=
            either (\e -> error $ "Can't parse JWords table " ++ e)
                   (return . buildMap number)
    checkMap m
    checkRefs m
    writeMap "foreign.csv" (possibleForeign m)
    writeMap "words.csv" m

    mconj <-  T.decodeFileL "../conjugations.txt" >>=
            either (\e -> error $ "Can't parse JConj table " ++ e)
                   (return . buildMap conjNumber)
    checkMap mconj
    let conjFormsQ = generateForms False mconj
        conjFormsA = generateForms True mconj
    T.writeFile "../test-forms.txt" (T.unlines conjFormsQ)
    T.writeFile "../test-formsA.txt" (T.unlines conjFormsA)
    (randFormsQ, randFormsA) <- fmap unzip . shuffleM $ zip conjFormsQ conjFormsA
    T.writeFile "../random-formsQ.txt" (T.unlines randFormsQ)
    T.writeFile "../random-formsA.txt" (T.unlines randFormsA)
    writeMap "conj.csv" mconj

    kw <- readFile "../kana.txt"
    let ks = concatMap fst (parseAll kw) :: [JKana]
    BL.writeFile "kana.csv" (encode ks)

