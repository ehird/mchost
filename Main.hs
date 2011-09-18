module Main where

import MC.Protocol
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize (Get)
import qualified Data.Serialize as SE
import System.IO
import System.Exit

printDump :: ByteString -> IO ()
printDump dump
  | B.null dump = return ()
  | otherwise =
    case SE.runGetState (SE.get :: Get ClientPacket) dump 0 of
      Left err -> hPutStr stderr err >> exitFailure
      Right (p, dump') -> print p >> printDump dump'

main :: IO ()
main = B.getContents >>= printDump
