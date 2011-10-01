{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import MC.Protocol
import qualified MC.Protocol.Client as C
import qualified MC.Protocol.Server as S
import Data.ByteString (ByteString)
import Data.Serialize (Get, Putter)
import qualified Data.Serialize as SE
import qualified Data.Text as T
import Data.IterIO
import qualified Data.IterIO.Iter as Iter
import Control.Applicative
import Control.Concurrent
import System.IO
import System.Environment
import System.Exit
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

handle :: (MonadIO m) => HostName -> PortNumber -> Inum [ClientPacket] [ServerPacket] m a
handle clientHost clientPort = mkInumAutoM $ do
  p <- headLI
  case p of
    C.ServerListPing -> kick ("server list" :: String)
    C.Handshake name -> do
      _ <- ifeed [S.Handshake (LoggedIn "hello")]
      login@C.Login{} <- headLI
      _ <- ifeed
        [ S.Login
            { S.loginEntity      = EntityID 42
            , S.loginUnused      = ""
            , S.loginMapSeed     = 0xFACADE
            , S.loginIsCreative  = 1
            , S.loginWorld       = WorldID 0
            , S.loginDifficulty  = Peaceful
            , S.loginWorldHeight = 128
            , S.loginMaxPlayers  = 1
            }
        ]
      kick (name, C.loginName login, C.loginVersion login)
    _ -> kick ("What *are* you?" :: String)
  where kick :: (MonadIO m, Show t) => t -> InumM [ClientPacket] [ServerPacket] m a ()
        kick info = do
          _ <- ifeed [S.Kick $ "ollies outy " `T.append` T.pack (show (info,clientHost,clientPort))]
          return ()

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
