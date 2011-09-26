{-# LANGUAGE TemplateHaskell #-}

module MC.Protocol.Client
  ( ClientPacket(..)
  ) where

import MC.Protocol.Template
import qualified MC.Protocol.Fields as PF

packetType "ClientPacket"
  [ packet 0x00 "KeepAlive"
    [ PF.int "cookie"
    ]
  , packet 0x01 "Login"
    [ PF.int "version"
    , PF.string "name"
    , PF.long "unused1"
    , PF.int "unused2"
    , PF.byte "unused3"
    , PF.byte "unused4"
    , PF.ubyte "unused5"
    , PF.ubyte "unused6"
    ]
  , packet 0x02 "Handshake"
    [ PF.string "name"
    ]
  , packet 0x03 "ChatMessage"
    [ PF.string "message"
    ]
  , packet 0x07 "UseEntity"
    [ PF.entityID "user"
    , PF.entityID "target"
    , PF.bool "isLeftClick"
    ]
  , packet 0x09 "Respawn"
    [ PF.worldID "world"
    , PF.byte "unused" -- difficulty in the server->client version; always sent as 1
    , PF.bool "isCreative" -- should probably be its own enum WorldType or whatever
    , PF.short "worldHeight"
    , PF.long "mapSeed" -- should probably have its own type too
    ]
  , packet 0x0A "Player"
    [ PF.bool "isOnGround" ]
  , packet 0x0B "PlayerPosition"
    [ PF.playerPos "playerPos"
    , PF.bool "isOnGround"
    ]
  , packet 0x0C "PlayerLook"
    [ PF.direction "direction"
    , PF.bool "isOnGround"
    ]
  , packet 0x0D "PlayerPositionLook"
    [ PF.playerPos "playerPos"
    , PF.direction "direction"
    , PF.bool "isOnGround"
    ]
  , packet 0x0E "PlayerDigging"
    [ PF.byte "status" -- should be PF.digStatus or whatever
    , PF.blockPos "blockPos"
    , PF.byte "face" -- should be PF.face or whatever
    ]
  , packet 0x0F "PlayerBlockPlacement"
    [ PF.blockPos "blockPos"
    , PF.byte "placeDirection" -- as above
    , PF.placement "placement"
    ]
  , packet 0x10 "HoldingChange"
    [ PF.short "slot"
    ]
  , packet 0x12 "Animation"
    [ PF.entityID "entity"
    , PF.byte "animation" -- as above, should be PF.animation
    ]
  , packet 0x13 "EntityAction"
    [ PF.entityID "entity"
    , PF.byte "action" -- as above, should be PF.action
    ]
    -- FIXME: is attach entity sent by the client?
    -- same for entity metadata
    -- also entity effect
    -- and remove entity effect
  , packet 0x65 "CloseWindow"
    [ PF.windowID "window" ]
  , packet 0x66 "WindowClick"
    [ PF.windowID "window"
    , PF.short "slot"
    , PF.bool "isRightClick"
      -- as above... this is a different action to the C.EntityAction
      -- packets; it's the same as the Transaction packet
    , PF.short "actionID"
    , PF.bool "shiftHeld"
    , PF.heldItem "item"
    ]
  , packet 0x6A "Transaction"
    [ PF.windowID "window"
    , PF.short "actionID" -- see C.WindowClick
    , PF.bool "accepted"
    ]
  , packet 0x82 "UpdateSign"
    [ PF.blockPos "blockPos"
    , PF.string "line1"
    , PF.string "line2"
    , PF.string "line3"
    , PF.string "line4"
    ]
  , packet 0xFE "ServerListPing" []
  , packet 0xFF "Disconnect"
    [ PF.string "unused"
    ]
  ]
