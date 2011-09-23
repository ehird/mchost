{-# LANGUAGE OverloadedStrings #-}

module Main where

import MC.Protocol
import Data.ByteString (ByteString)
import Data.Serialize (Get, Putter)
import qualified Data.Serialize as SE
import qualified Data.Text as T
import Data.IterIO
import qualified Data.IterIO.Iter as Iter
import Control.Applicative
import System.IO
import System.Environment
import System.Exit
import Control.Concurrent
import Network

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

handle :: HostName -> PortNumber -> Inum [ClientPacket] [ServerPacket] IO a
handle clientHost clientPort = mkInum $ do
  return [SKick $ "ollies outy " `T.append` T.pack (show (clientHost,clientPort))]

serverLoop :: Socket -> IO ()
serverLoop server = loop
  where loop = do
          (client, clientHost, clientPort) <- accept server
          hSetBuffering client NoBuffering
          (clientI, enumClient) <- iterHandle client
          putStrLn $ "Handling connection from " ++ show (clientHost,clientPort)
          _ <- forkIO . Iter.run
             $ enumClient
            .| getI SE.get
            .| handle clientHost clientPort
            .| enumPut SE.put
            .| clientI
          loop

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  port <- case map reads args of
    [[(n,"")]] -> return $ fromIntegral (n :: Int)
    _ -> hPutStrLn stderr "usage: mchost <port>" >> exitFailure
  server <- listenOn (PortNumber port)
  putStrLn $ "Listening on port " ++ show port ++ "..."
  serverLoop server
