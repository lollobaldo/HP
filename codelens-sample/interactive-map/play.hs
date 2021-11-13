{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts          #-}

class PrettyPrintable t where
  f :: t -> Int 


instance (Num a, Show a) => PrettyPrintable a where
  f _ = 0


instance {-# OVERLAPPABLE #-} (PrettyPrintable a, Show a) => PrettyPrintable [a] where
  f _ = 1


h :: PrettyPrintable t => t -> Int
h = f

b :: [Int]
b = [1,2,3]
a = h b
