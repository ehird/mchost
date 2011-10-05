module MC.Utils
  ( showConcatMap
  ) where

showConcatMap :: (a -> ShowS) -> [a] -> ShowS
showConcatMap = flip . foldr
