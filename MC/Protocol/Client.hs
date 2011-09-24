{-# LANGUAGE TemplateHaskell #-}

module MC.Protocol.Client
  ( ClientPacket(..)
  ) where

import MC.Protocol.Template
import qualified MC.Protocol.Fields as PF

packetType "ClientPacket"
  [ packet 0x00 "CKeepAlive"
    [ PF.int "id"
    ]
  , packet 0x01 "CLogin"
    [ PF.int "version"
    , PF.string "name"
    , PF.long "unused1"
    , PF.int "unused2"
    , PF.byte "unused3"
    , PF.byte "unused4"
    , PF.ubyte "unused5"
    , PF.ubyte "unused6"
    ]
  , packet 0x02 "CHandshake"
    [ PF.string "name"
    ]
  , packet 0x03 "CChatMessage"
    [ PF.string "message"
    ]
  , packet 0x07 "CUseEntity"
    [ PF.entityID "user"
    , PF.entityID "target"
    , PF.bool "isLeftClick"
    ]
  , packet 0x09 "CRespawn"
    [ PF.worldID "world"
    , PF.byte "unused" -- difficulty in the server->client version; always sent as 1
    , PF.bool "isCreative" -- should probably be its own enum WorldType or whatever
    , PF.short "worldHeight"
    , PF.long "mapSeed" -- should probably have its own type too
    ]
  , packet 0x0A "CPlayer"
    [ PF.bool "isOnGround" ]
  , packet 0x0B "CPlayerPosition"
    [ PF.double "x"
    , PF.double "y"
    , PF.double "stance"
    , PF.double "z"
    , PF.bool "isOnGround"
    ]
  , packet 0x0C "CPlayerLook"
    [ PF.float "yaw"
    , PF.float "pitch"
    , PF.bool "isOnGround"
    ]
  , packet 0x0D "CPlayerPositionLook"
    [ PF.double "x"
    , PF.double "y"
    , PF.double "stance"
    , PF.double "z"
    , PF.float "yaw"
    , PF.float "pitch"
    , PF.bool "isOnGround"
    ]
  , packet 0x0E "CPlayerDigging"
    [ PF.byte "status" -- should be PF.digStatus or whatever
    , PF.int "x"
    , PF.byte "y"
    , PF.int "z"
    , PF.byte "face" -- should be PF.face or whatever
    ]
  , packet 0x0F "CPlayerBlockPlacement"
    [ PF.int "x"
    , PF.byte "y"
    , PF.int "z"
    , PF.byte "direction" -- as above
    , PF.placement "placement"
    ]
  , packet 0x10 "CHoldingChange"
    [ PF.short "slot"
    ]
  , packet 0x12 "CAnimation"
    [ PF.entityID "entity"
    , PF.byte "animation" -- as above, should be PF.animation
    ]
  , packet 0x13 "CEntityAction"
    [ PF.entityID "entity"
    , PF.byte "action" -- as above, should be PF.action
    ]
    -- FIXME: is attach entity sent by the client?
    -- same for entity metadata
    -- also entity effect
    -- and remove entity effect
  , packet 0x65 "CCloseWindow"
    [ PF.windowID "window" ]
  , packet 0x66 "CWindowClick"
    [ PF.windowID "window"
    , PF.short "slot"
    , PF.bool "isRightClick"
      -- as above... this is a different action to the CEntityAction
      -- packets; it's the same as the Transaction packet
    , PF.short "actionID" 
    , PF.bool "shiftHeld"
    , PF.heldItem "item"
    ]
  , packet 0x6A "CTransaction"
    [ PF.windowID "window"
    , PF.short "actionID" -- see CWindowClick
    , PF.bool "accepted"
    ]
  , packet 0x82 "CUpdateSign"
    [ PF.int "x"
    , PF.short "y"
    , PF.int "z"
    , PF.string "line1"
    , PF.string "line2"
    , PF.string "line3"
    , PF.string "line4"
    ]
  , packet 0xFE "CServerListPing" []
  , packet 0xFF "CDisconnect"
    [ PF.string "unused"
    ]
  ]