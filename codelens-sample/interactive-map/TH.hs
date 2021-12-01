{-# LANGUAGE TemplateHaskell #-}

import Data.List

import Language.Haskell.TH
import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Language.Haskell.Meta

-- | Pretty print spliced code
pprintQ :: Ppr a => Q a -> IO ()
pprintQ q = runQ q >>= putStrLn.pprint

ppprint :: String -> String
ppprint = go 0
  where
    go n ('(':xs) = "(\n" ++ replicate (n+2) ' ' ++ go (n+2) xs
    go n ('[':xs) = "[\n" ++ replicate (n+2) ' ' ++ go (n+2) xs
    go n ('{':xs) = "{\n" ++ replicate (n+2) ' ' ++ go (n+2) xs
    -- go n ('{':xs) = "{" ++ go (n+2) (dropWhile (/= '}') xs)
    go n (')':xs) = ")\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n (']':xs) = "]\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n ('}':xs) = "}\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n (x:xs)   = x : go n xs
    go n []       = []


parseRes :: ParseResult a -> a
parseRes (ParseOk a) = a

parseMod :: Module l -> [Decl l]
parseMod (Module l _ _ _ ds) = ds

-- parseDec :: Decl l -> String
parseDec (PatBind _ (PVar _ (Ident s n)) _ _) = (s,n)
parseDec _ = undefined

useDecl :: Decl l -> Decl l
useDecl = undefined

h :: Num a => [a]
h = [1, 1.5, 1%4]

processor :: String -> IO ()
processor file = do
  pr <- parseFile file
  let mod = parseRes pr
  let ds = parseMod mod
  let ns = map parseDec ds
  print $ exactPrint (head ds) []
  print ns
  let str = ppprint (show ns)
  writeFile "temp.txt" str

main = do
  pr <- parseFile "play.hs"
  let mod = parseRes pr
  let ds = parseMod mod
  print ds
  let str = ppprint (show ds)
  writeFile "temp.txt" str
