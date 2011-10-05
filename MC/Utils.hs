module MC.Utils
  ( showConcatMap
  , cutoff
  ) where

showConcatMap :: (a -> ShowS) -> [a] -> ShowS
showConcatMap = flip . foldr

-- | Fits a string to a certain length, replacing the last characters
-- with the specified terminator if the string is too long.
--
-- > cutoff "..." 8 "abcd"      == "abcd"
-- > cutoff "..." 8 "abcdabcd"  == "abcdabcd"
-- > cutoff "..." 8 "abcdabcda" == "abcda..."
cutoff :: String -> Int -> String -> String
cutoff end m
  | m < l = error "cutoff: target length shorter than terminator string"
  | otherwise = cutoff' (m - l)
  where l = length end
        cutoff' 0 xs
          | null (drop l xs) = xs
          | otherwise = end
        cutoff' _ [] = []
        cutoff' n (x:xs) = x : cutoff' (n-1) xs
