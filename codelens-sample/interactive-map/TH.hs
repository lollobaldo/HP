{-# LANGUAGE TemplateHaskell #-}

import Data.List

import Language.Haskell.TH
import Language.Haskell.Exts
import Language.Haskell.Meta

-- | Pretty print spliced code
pprintQ :: Ppr a => Q a -> IO ()
pprintQ q = runQ q >>= putStrLn.pprint

ppprint :: String -> String
ppprint = go 0
  where
    go n ('(':xs) = "(\n" ++ replicate (n+2) ' ' ++ go (n+2) xs
    go n ('[':xs) = "[\n" ++ replicate (n+2) ' ' ++ go (n+2) xs
    go n ('{':xs) = "{" ++ go (n+2) (dropWhile (/= '}') xs)
    go n (')':xs) = ")\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n (']':xs) = "]\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n ('}':xs) = "}\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n (x:xs)   = x : go n xs
    go n []       = []

parseMod :: Module l -> [Dec]
parseMod (Module l _ _ _ ds) = map toDec ds

parseRes :: ParseResult a -> a
parseRes (ParseOk a) = a

main = do
  pr <- parseFile "play.hs"
  let mod = parseRes pr
  let ds = parseMod mod
  print ds
  let str = ppprint (show (ds))
  writeFile "temp.txt" str

-- let isPrime :: (Integral a) => a -> Bool
--     isPrime k | k <=1 = False | otherwise = not $ elem 0 (map (mod k)[2..k-1])

-- let nextPrime :: (Integral a) => a -> a
--     nextPrime n | isPrime n = n | otherwise = nextPrime (n+1)

-- -- returns a list of all primes between n and m, using the nextPrime function
-- let doPrime :: (Integral a) => a -> a -> [a]
--     doPrime n m
--         | curr > m = []
--         | otherwise = curr:doPrime (curr+1) m
--         where curr = nextPrime n

-- -- and our Q expression
-- let primeQ :: Int -> Int -> Q Exp
--     primeQ n m = [| doPrime n m |]