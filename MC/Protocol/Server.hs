{-# LANGUAGE TemplateHaskell #-}

module MC.Protocol.Server
  ( ServerPacket(..)
  ) where

import MC.Protocol.Template
import qualified MC.Protocol.Fields as PF

packetType "ServerPacket"
  [ packet 0x00 "KeepAlive"
    [ PF.int "cookie"
    ]
  , packet 0x01 "Login"
    [ PF.entityID "entity"
    , PF.string "unused"
    , PF.long "mapSeed" -- as above
      -- Note that this is an *int*, not a bool.
      --
      -- FIXME: Give this its own type too.
    , PF.int "isCreative"
    , PF.worldID "world"
    , PF.difficulty "difficulty"
    , PF.ubyte "worldHeight"
    , PF.ubyte "maxPlayers"
    ]
  , packet 0x02 "Handshake"
    [ PF.serverHandshake "handshake" -- FIXME: handshake is a rubbish name (previously value)
    ]
  , packet 0x03 "ChatMessage"
    [ PF.string "message"
    ]
  , packet 0x04 "TimeUpdate"
    [ PF.long "time" -- should maybe have its own type too?
    ]
  , packet 0x05 "EntityEquipment"
    [ PF.entityID "entity"
    , PF.short "slot" -- this too
    , PF.equipment "equipment"
    ]
  , packet 0x06 "SpawnPosition"
    [ PF.blockPosIntY "position"
    ]
  , packet 0x08 "UpdateHealth"
    [ PF.short "health"
    , PF.short "food"
    , PF.float "foodSaturation"
    ]
    -- Identical to the client version; all notes for that one apply
    -- here too.
  , packet 0x09 "Respawn"
    [ PF.worldID "world"
    , PF.difficulty "difficulty"
    , PF.bool "isCreative"
    , PF.short "worldHeight"
    , PF.long "mapSeed"
    ]
    -- NOTE: This packet is NOT the same as the client version; the Y
    -- coordinate and the stance value are swapped. Yes, this is
    -- completely ridiculous.
  , packet 0x0D "PlayerPositionLook"
    [ PF.playerPosXSYZ "position"
    , PF.direction "direction"
    , PF.bool "isOnGround"
    ]
  , packet 0x11 "UseBed"
    [ PF.entityID "entity"
    , PF.byte "unknown" -- "???In Bed???", "0 Appears when players use bed"
    , PF.blockPos "position" -- "headboard?"
    ]
    -- FIXME: Does the server ever send these? I think yes, but I'm
    -- not sure.
  , packet 0x12 "Animation"
    [ PF.entityID "entity"
    , PF.byte "animation" -- as above
    ]
  , packet 0x14 "NamedEntitySpawn"
    [ PF.entityID "entity"
    , PF.string "name"
    , PF.intPoint "position"
    , PF.byteDirection "direction"
      -- I don't think we need to worry about sending this awful thing
      -- as anything but "nothing held"; we can send it as an
      -- equipment packet instead. Which is a relief, because it's
      -- horrible.
    , PF.currentItem "currentItem"
    ]
  , packet 0x15 "PickupSpawn"
    [ PF.entityID "entity"
      -- FIXME: This makes the name heldItem quite inaccurate, but I
      -- can't think of anything better.
    , PF.heldItem "item"
    , PF.intPoint "position"
    , PF.byteOrientation "orientation"
    ]
  , packet 0x16 "CollectItem"
    -- FIXME: These names suck.
    [ PF.entityID "collectedEntity"
    , PF.entityID "collectingEntity"
    ]
  , packet 0x17 "ObjectSpawn"
    [ PF.entityID "entity"
    , PF.byte "type" -- should have its own type
    , PF.intPoint "position"
    , PF.fireball "fireball"
    ]
  , packet 0x18 "MobSpawn"
    [ PF.entityID "entity"
    , PF.byte "type" -- as above
    , PF.intPoint "position"
    , PF.byteDirection "direction"
    , PF.entityData "data"
    ]
  , packet 0x19 "PaintingSpawn"
    [ PF.entityID "entity"
    , PF.string "title"
    , PF.blockPosIntY "centrePos"
    , PF.int "direction" -- should have its own type
    ]
  , packet 0x1A "ExperienceOrbSpawn"
    [ PF.entityID "entity"
    , PF.intPoint "position"
    , PF.short "count"
    ]
  , packet 0x1C "EntityVelocity"
    [ PF.entityID "entity"
      -- These should possibly have their own type
    , PF.short "dx"
    , PF.short "dy"
    , PF.short "dz"
    ]
  , packet 0x1D "DestroyEntity"
    [ PF.entityID "entity"
    ]
  , packet 0x1E "Entity"
    [ PF.entityID "entity"
    ]
  , packet 0x1F "EntityRelativeMove"
    [ PF.entityID "entity"
      -- These should possibly use the same type as S.EntityVelocity,
      -- but they're a different size, and represent a difference, not
      -- a velocity, so maybe not. (Even if the difference between the
      -- two is questionable, the entity velocity packets are relative
      -- to a time delta.)
    , PF.byte "dx"
    , PF.byte "dy"
    , PF.byte "dz"
    ]
  , packet 0x20 "EntityLook"
    [ PF.entityID "entity"
    , PF.byteDirection "direction"
    ]
  , packet 0x21 "EntityLookRelativeMove"
    [ PF.entityID "entity"
      -- as above
    , PF.byte "dx"
    , PF.byte "dy"
    , PF.byte "dz"
    , PF.byteDirection "direction"
    ]
  , packet 0x22 "EntityTeleport"
    [ PF.entityID "entity"
    , PF.intPoint "position"
    , PF.byteDirection "direction"
    ]
  , packet 0x26 "EntityStatus"
    [ PF.entityID "entity"
    , PF.byte "status" -- should have its own type
    ]
  , packet 0x27 "AttachEntity"
    [ PF.entityID "attachedEntity"
      -- TODO FIXME: This is wrong! It is either deattaching, or an
      -- entity ID.
    , PF.entityID "vehicleEntity"
    ]
  , packet 0x28 "EntityMetadata"
    [ PF.entityID "entity"
      -- as S.MobSpawn; maybe rename this too?
    , PF.entityData "data"
    ]
  , packet 0x29 "EntityEffect"
    [ PF.entityID "entity"
    , PF.byte "effect" -- should have its own type
    , PF.byte "amplifier"
    , PF.short "duration"
    ]
  , packet 0x2A "RemoveEntityEffect"
    [ PF.entityID "entity"
    , PF.byte "effect" -- as above
    ]
  , packet 0x2B "Experience"
    [ PF.byte "currentExperience"
    , PF.byte "experienceLevel"
    , PF.short "totalExperience"
    ]
  , packet 0x32 "PreChunk"
    [ PF.chunkPos "chunkPos"
      -- This should have its own type. "If mode is 0 the client will
      -- unload the chunk, otherwise the client will initialize the
      -- chunk"
    , PF.bool "mode"
    ]
  , packet 0x33 "MapChunk"
    [ -- This is the block position of the least-X, least-Y, least-Z
      -- block in the data being sent.
      PF.blockPosShortY "point"
    , PF.byte "xSize"
    , PF.byte "ySize"
    , PF.byte "zSize"
    , PF.mapChunk "data"
    ]
    -- This packet is completely awful.
  , packet 0x34 "MultiBlockChange"
    [ PF.chunkPos "chunkPos"
    , PF.multiBlockChangeData "data"
    ]
  , packet 0x35 "BlockChange"
    [ PF.blockPos "blockPos"
    , PF.block "block"
    ]
  , packet 0x36 "BlockAction"
    [ PF.blockPosShortY "blockPos"
      -- This packet is horrible in every way; the fields are state
      -- and direction if this is a piston, but instrument and pitch
      -- if it's a note block. We can't just make this into a data
      -- type because its meaning depends on the state of the map.
    , PF.byte "data1"
    , PF.byte "data2"
    ]
  , packet 0x3C "Explosion"
    [ PF.point "position"
    , PF.float "unknown" -- wiki says this might be radius
    , PF.explosionData "data"
    ]
  , packet 0x3D "SoundEffect"
    [ PF.int "effect" -- as above
    , PF.blockPos "blockPos"
      -- should be a custom type, but since it depends on the effect
      -- this whole packet will have to become a custom type; FIXME:
      -- is there a solution that isn't that ugly?
    , PF.int "data"
    ]
  , packet 0x46 "NewOrInvalidState"
    [ PF.byte "reason" -- should have its own type
    , PF.bool "isCreative" -- should have its own type, as above
    ]
  , packet 0x47 "Thunderbolt"
    [ PF.entityID "entity"
    , PF.bool "unknown" -- always true
    , PF.intPoint "position"
    ]
  , packet 0x64 "OpenWindow"
    [ PF.windowID "window"
    , PF.byte "type" -- should have its own type
    , PF.string "title"
    , PF.byte "slots"
    ]
  , packet 0x65 "CloseWindow"
    [ PF.windowID "window"
    ]
  , packet 0x67 "SetSlot"
    [ PF.windowID "window" -- actually window ID or 0 for player inventory; this should have its own type
    , PF.short "slot"
    , PF.maybeHeldItem "item"
    ]
  , packet 0x68 "WindowItems"
    [ PF.windowID "window" -- as above
    , PF.windowItems "items"
    ]
  , packet 0x69 "UpdateProgressBar"
    [ PF.windowID "window" -- as above, I think
    , PF.short "bar" -- should have its own type...
    , PF.short "value"
    ]
  , packet 0x6A "Transaction"
    [ PF.windowID "window" -- probably as above; not sure
    , PF.short "actionID" -- should have its own type, as (far) above
    , PF.bool "accepted"
    ]
    -- Identical to the client version.
  , packet 0x82 "UpdateSign"
    [ PF.blockPosShortY "blockPos"
    , PF.string "line1"
    , PF.string "line2"
    , PF.string "line3"
    , PF.string "line4"
    ]
    -- Called "Item Data" on the wiki.
  , packet 0x83 "MapData"
    [ PF.item "item"
    , PF.mapData "data"
    ]
  , packet 0xC8 "IncrementStatistic"
    [ PF.int "statistic" -- should have its own type
    , PF.byte "amount"
    ]
  , packet 0xC9 "PlayerListItem"
    [ PF.string "name"
    , PF.bool "online"
    , PF.short "ping" -- in ms
    ]
  , packet 0xFF "Kick"
    [ PF.string "reason"
    ]
  ]
