{-# LANGUAGE OverloadedStrings #-}

module MC.Host
  ( runServer
  ) where

import MC.Protocol
import qualified MC.Protocol.Client as C
import qualified MC.Protocol.Server as S
import MC.Utils

import Prelude hiding (catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.IterIO
import qualified Data.IterIO as Iter
import qualified Data.Serialize as SE
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Concurrent
import Data.IORef
import System.IO
import Network
import Codec.Zlib

data ConnInfo = ConnInfo
  { connClientI    :: Iter ByteString IO ()
  , connEnumClient :: Onum ByteString IO ()
  , connHost       :: HostName
  , connPort       :: PortNumber
  }

compress :: ByteString -> IO ByteString
compress bs = do
  chunks <- newIORef B.empty
  def <- initDeflate 9 defaultWindowBits
  let handler = handleData chunks
  withDeflateInput def bs handler
  finishDeflate def handler
  readIORef chunks
  where handleData chunks getData = do
          m <- getData
          case m of
            Nothing -> return ()
            Just more -> modifyIORef chunks (`B.append` more)

uncompressedTestChunk :: ByteString
uncompressedTestChunk = B.pack $ concat
  [ concat $ replicate 256 (replicate 62 dirt ++ grass : replicate 65 air)
  , replicate 16384 0
  , replicate 16384 0xff
  , replicate 16384 0xff
  ]
  where air = 0
        grass = 2
        dirt = 3

serve :: (MonadIO m) => ConnInfo -> Inum [ClientPacket] [ServerPacket] m a
serve conn = mkInumAutoM $ do
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
        , S.ChatMessage . T.pack $ show (name, C.loginName login, C.loginVersion login)
        ]
      sequence_ [ sendChunk (ChunkPos cx cz) | cx <- [-3..3], cz <- [-3..3] ]
      _ <- ifeed
        [ S.SpawnPosition (Point 0 64 0)
        , S.WindowItems (WindowID 0) (WindowItems (replicate 45 Nothing))
        , S.SetSlot (WindowID (-1)) (-1) Nothing
        , S.PlayerPositionLook (PlayerPos (Point 0 64 0) 71.62) (Direction 0 0) True
        ]
      forever $ do
        _ <- ifeed [S.KeepAlive 0]
        -- This makes us buffer an unbounded amount of data as the
        -- client keeps sending stuff while we wait and we only read
        -- one packet here, but it doesn't really matter since this is
        -- just a test.
        _ <- dataI
        liftIO $ threadDelay 1000000
    _ -> kick ("What *are* you?" :: String)
  where kick :: (MonadIO m, Show t) => t -> InumM [ClientPacket] [ServerPacket] m a ()
        kick info = do
          _ <- ifeed [S.Kick $ "ollies outy " `T.append` T.pack (show (info, connHost conn, connPort conn))]
          return ()
        chunkToBlock :: ChunkPos -> Point
        chunkToBlock (ChunkPos cx cz) = Point (fromIntegral cx * 16) 0 (fromIntegral cz * 16)
        sendChunk :: (MonadIO m) => ChunkPos -> InumM [ClientPacket] [ServerPacket] m a ()
        sendChunk chunkPos = do
          compressedChunk <- liftIO $ compress uncompressedTestChunk
          _ <- ifeed
            [ S.PreChunk chunkPos True
            , S.MapChunk
                { S.mapChunkPoint = chunkToBlock chunkPos
                , S.mapChunkXSize = 15
                , S.mapChunkYSize = 127
                , S.mapChunkZSize = 15
                , S.mapChunkData  = MapChunk compressedChunk
                }
            ]
          return ()

acceptConn :: Socket -> IO ConnInfo
acceptConn server = do
  (client, clientHost, clientPort) <- accept server
  hSetBuffering client NoBuffering
  (clientI, enumClient) <- iterHandle client
  return ConnInfo
    { connClientI    = clientI
    , connEnumClient = enumClient
    , connHost       = clientHost
    , connPort       = clientPort
    }

showConnAddress :: ConnInfo -> ShowS
showConnAddress conn =
  showString (connHost conn) .
  showChar ':' .
  shows (connPort conn)

showNetPacket :: (Packet a) => ConnInfo -> Char -> a -> ShowS
showNetPacket conn sym p = showString ((cutoff "..." width . line) "") . showString "\n"
  where width = 128
        line = showConnAddress conn . showChar ' '. showChar sym . showChar ' ' . showPacketInline p

runServer :: Socket -> IO ()
runServer server = forever $ do
  conn <- acceptConn server
  putLogLn conn "Starting connection."
  forkIO $ Iter.run (iter conn) `finally` finish conn
  where iter conn = connEnumClient conn
                 .| getI SE.get
                 .| inumTee (showsI (showNetPacket conn '<') .| stdoutI)
                 .| serve conn
                 .| inumTee (showsI (showNetPacket conn '>') .| stdoutI)
                 .| enumPut SE.put
                 .| connClientI conn
        finish conn = putLogLn conn "Finishing connection."
        putLogLn conn = putStrLn . showConnAddress conn . showString " - "
