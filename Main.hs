module Main where

import MC.Protocol
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize (Get)
import qualified Data.Serialize as SE
import System.IO
import System.Environment
import System.Exit

printDump :: (Show a) => Get a -> ByteString -> IO ()
printDump get dump
  | B.null dump = return ()
  | otherwise =
    case SE.runGetState get dump 0 of
      Left err -> hPutStr stderr err >> exitFailure
      Right (p, dump') -> print p >> printDump get dump'

main :: IO ()
main = do
  [origin] <- getArgs
  dump <- B.getContents
  case origin of
    "client" -> printDump (SE.get :: Get ClientPacket) dump
    "server" -> printDump (SE.get :: Get ServerPacket) dump
    _ -> hPutStrLn stderr "usage: mchost {client,server} < dump" >> exitFailure
