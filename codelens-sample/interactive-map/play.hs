{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts          #-}

class Displayable t where
  f :: t -> Int 


instance (Num a, Show a) => Displayable a where
  f _ = 0


instance {-# OVERLAPPABLE #-} (Displayable a, Show a) => Displayable [a] where
  f _ = 1


h :: Displayable t => t -> Int
h = f

b :: [Int]
b = [1,2,3]
a = h b
