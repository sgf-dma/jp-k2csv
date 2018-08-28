
import Control.Monad (replicateM)
import Control.Monad.State
import Text.Printf
import System.Random
import System.Random.Shuffle
import System.Directory


-- Number of items from certain range.
num :: Int
num = 50

-- How many ranges to take.
range :: Int
range = 3

-- Filename without extension and numeric suffix.
fname :: FilePath
fname = "random-nums"

main :: IO ()
main = do
    g <- newStdGen
    let ds = go g num range
    ds' <- unlines . map show <$> shuffleM ds
    fn  <- getFilename
    writeFile fn ds'
    putStr ds'
  where
    --go :: RandomGen g => Int -> Int -> [Int]
    go g n m =
        flip evalState g
            . fmap concat
            . mapM (generate n)
            . take m
            $ [ (10    , 99)
              , (100   , 999)
              , (1000  , 9999)
              , (10000 , 99999)
              , (100000, 999999)
              ]

-- Generate random numbers in given range.
generate :: RandomGen g => Int -> (Int, Int) -> State g [Int]
generate n = replicateM n . state . randomR

-- Find not yet used filename.
getFilename :: IO FilePath
getFilename = go 1
  where
    go :: Int -> IO FilePath
    go x = do
        let fn = fname ++ "-" ++ printf "%03d" x ++ ".txt"
        b <- doesFileExist fn
        if b then go (x + 1) else return fn

