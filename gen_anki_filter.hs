
import Data.List

ns = take 50 $ [1..]

kanji_wout_refs :: Show a => [a] -> [String]
kanji_wout_refs = map (\x -> "(ref:*S" ++ show x ++ "-K* -ref:*S" ++ show x ++ "-K-R*)")

triples :: [a] -> [[a]]
triples (x0 : y0 : zs) = foldr (\x z y w -> (w : y : [x]) : z x y) (\y w -> [[w, y]]) zs y0 x0

build :: [[String]] -> [String]
build       = map (\xs -> "deck:jp::words (" ++ intercalate " or " xs ++ ") card:reading")
                . map snd . filter fst . zip (cycle [True, False, False])

main :: IO ()
main    = mapM_ putStrLn . build . triples . kanji_wout_refs $ ns
