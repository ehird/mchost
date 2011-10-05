module Main
  ( main
  ) where

import MC.Protocol
import MC.Host
import MC.Utils

import Data.Serialize (Get)
import qualified Data.Serialize as SE
import Data.IterIO
import qualified Data.IterIO.Iter as Iter
import System.IO
import System.Environment
import System.Exit
import Network

analyse :: (Packet a, Show a) => Get a -> FilePath -> IO ()
analyse get filename = withBinaryFile filename ReadMode $ \h ->
  Iter.run $ enumHandle h .| getI get .| showsI (\p -> showPacketInline p . showString "\n") .| stdoutI

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--analyse-client", filename] -> analyse (SE.get :: Get ClientPacket) filename
    ["--analyse-server", filename] -> analyse (SE.get :: Get ServerPacket) filename
    [ns] | [(n,"")] <- reads ns -> withSocketsDo $ do
      let port = fromIntegral (n :: Int) :: PortNumber
      server <- listenOn (PortNumber port)
      putStrLn $ "Listening on port " ++ show port ++ "..."
      runServer server
    _ -> hPutStrLn stderr "usage: mchost {<port> | --analyse-{client|server} <filename>}" >> exitFailure
