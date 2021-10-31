{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module PR where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Debug.Trace

class Show a => PR a where
    pr :: a -> IO ()
instance PR Text where
    pr = T.putStrLn
instance Show a => PR a where
    pr = print
