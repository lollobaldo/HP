module Crud where

import Displayable

data Crud = Create | Update | Delete

data Op a = Op Crud Key a

crudList :: Op a -> [(Key, a)] -> [a]
crudList (Op Create key a) ls = concatMap (\(k, e) -> if k == key then [e,a] else [e]) ls
crudList (Op Update key a) ls = map (\(k, e) -> if k == key then a else e) ls
crudList (Op Delete key _) ls = concatMap (\(k, e) -> if k == key then [] else [e]) ls

