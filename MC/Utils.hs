module MC.Utils
  ( showConcatMap
  , cutoff
  , showsI
  , getI
  , enumPut
  ) where

import Data.ByteString (ByteString)
import Data.Serialize (Get, Putter)
import qualified Data.Serialize as SE
import Data.IterIO
import Control.Applicative

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

showsI :: (Monad m, Show t) => (t -> ShowS) -> Inum [t] String m a
showsI disp = mkInum $ fmapI (\xs -> showConcatMap disp xs "") dataI

getI :: (Monad m, Show t) => Get t -> Inum ByteString [t] m a
getI m = mkInum $ loop (SE.runGetPartial m)
  where loop continue = do
          str <- dataI
          case continue str of
            SE.Fail err -> throwParseI err
            SE.Partial k -> loop k
            SE.Done a s -> ungetI s >> return [a]

enumPut :: (Monad m, Show t) => Putter t -> Inum [t] ByteString m a
enumPut f = mkInum (SE.runPut . mapM_ f <$> dataI)
