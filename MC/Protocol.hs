{-# LANGUAGE TemplateHaskell #-}

module MC.Protocol
  ( ClientPacket(..)
  , ServerPacket(..)
  , module MC.Protocol.Types
  ) where

import MC.Protocol.Template
import MC.Protocol.Types
import qualified MC.Protocol.Fields as PF

-- All the various coordinate formats used below should have their own
-- field type that parses to a reasonable coordinate type (there can
-- be multiple, as long as they're all reasonable).

packetType "ClientPacket"
  [ packet 0x00 "CKeepAlive"
    [ PF.int "id"
    ]
  , packet 0x01 "CLogin"
    [ PF.int "version"
    , PF.string "name"
    , PF.long "unused"
    , PF.int "unused"
    , PF.byte "unused"
    , PF.byte "unused"
    , PF.ubyte "unused"
    , PF.ubyte "unused"
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
    , PF.short "action" -- as above... this may be a different action, it's the same as the Transaction packet apparently
    , PF.bool "shiftHeld"
    , PF.heldItem "item"
    ]
  , packet 0x6A "CTransaction"
    [ PF.windowID "window"
    , PF.short "action" -- see CWindowClick
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

packetType "ServerPacket"
  [ packet 0x00 "SKeepAlive"
    [ PF.int "id" ]
  , packet 0x01 "SLogin"
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
  , packet 0x02 "SHandshake"
    [ PF.serverHandshake "handshake" -- FIXME: handshake is a rubbish name (previously value)
    ]
  , packet 0x03 "SChatMessage"
    [ PF.string "message"
    ]
  , packet 0x04 "STimeUpdate"
    [ PF.long "time" -- should maybe have its own type too?
    ]
  , packet 0x05 "SEntityEquipment"
    [ PF.entityID "entity"
    , PF.short "slot" -- this too
    , PF.equipment "equipment"
    ]
  , packet 0x06 "SSpawnPosition"
    [ PF.int "x"
    , PF.int "y"
    , PF.int "z"
    ]
  , packet 0x08 "SUpdateHealth"
    [ PF.short "health"
    , PF.short "food"
    , PF.float "foodSaturation"
    ]
    -- Identical to the client version; all notes for that one apply
    -- here too.
  , packet 0x09 "SRespawn"
    [ PF.worldID "world"
    , PF.difficulty "difficulty"
    , PF.bool "isCreative"
    , PF.short "worldHeight"
    , PF.long "mapSeed"
    ]
    -- NOTE: This packet is NOT the same as the client version; the Y
    -- coordinate and the stance value are swapped. Yes, this is
    -- completely ridiculous.
  , packet 0x0D "SPlayerPositionLook"
    [ PF.double "x"
    , PF.double "stance"
    , PF.double "y"
    , PF.double "z"
    , PF.float "yaw"
    , PF.float "pitch"
    , PF.bool "isOnGround"
    ]
  , packet 0x11 "SUseBed"
    [ PF.entityID "entity"
    , PF.byte "unknown" -- "???In Bed???", "0 Appears when players use bed"
    , PF.int "x"
    , PF.byte "y"
    , PF.int "z"
    ]
    -- FIXME: Does the server ever send these? I think yes, but I'm
    -- not sure.
  , packet 0x12 "SAnimation"
    [ PF.entityID "entity"
    , PF.byte "animation" -- as above
    ]
  , packet 0x14 "SNamedEntitySpawn"
    [ PF.entityID "entity"
    , PF.string "name"
    , PF.int "x"
    , PF.int "y"
    , PF.int "z"
      -- FIXME: Rotation and pitch are "packed bytes", whatever that
      -- means; change types accordingly
    , PF.byte "rotation"
    , PF.byte "pitch"
      -- I don't think we need to worry about sending this awful thing
      -- as anything but "nothing held"; we can send it as an
      -- equipment packet instead. Which is a relief, because it's
      -- horrible.
    , PF.currentItem "currentItem"
    ]
  , packet 0x15 "SPickupSpawn"
    [ PF.entityID "entity"
      -- FIXME: This makes the name heldItem quite inaccurate, but I
      -- can't think of anything better.
    , PF.heldItem "item"
    , PF.int "x"
    , PF.int "y"
    , PF.int "z"
      -- Packed bytes as above, including roll.
    , PF.byte "rotation"
    , PF.byte "pitch"
    , PF.byte "roll"
    ]
  , packet 0x16 "SCollectItem"
    -- FIXME: These names suck.
    [ PF.entityID "collectedEntity"
    , PF.entityID "collectingEntity"
    ]
  , packet 0x17 "SObjectSpawn"
    [ PF.entityID "entity"
    , PF.byte "type" -- should have its own type
    , PF.int "x"
    , PF.int "y"
    , PF.int "z"
    , PF.fireball "fireball"
    ]
  , packet 0x18 "SMobSpawn"
    [ PF.entityID "entity"
    , PF.byte "type" -- as above
    , PF.int "x"
    , PF.int "y"
    , PF.int "z"
      -- Apparently these are in steps of 2pi/256. Perhaps this is the
      -- packed format mentioned above.
    , PF.byte "yaw"
    , PF.byte "pitch"
      -- FIXME: Possibly rename to "data"?
    , PF.entityData "metadata"
    ]
  , packet 0x19 "SPaintingSpawn"
    [ PF.entityID "entity"
    , PF.string "title"
    , PF.int "x"
    , PF.int "y"
    , PF.int "z"
    , PF.int "direction" -- should have its own type
    ]
  , packet 0x1A "SExperienceOrbSpawn"
    [ PF.entityID "entity"
    , PF.int "x"
    , PF.int "y"
    , PF.int "z"
    , PF.short "count"
    ]
  , packet 0x1C "SEntityVelocity"
    [ PF.entityID "entity"
      -- These should possibly have their own type
    , PF.short "dx"
    , PF.short "dy"
    , PF.short "dz"
    ]
  , packet 0x1D "SDestroyEntity"
    [ PF.entityID "entity"
    ]
  , packet 0x1E "SEntity"
    [ PF.entityID "entity"
    ]
  , packet 0x1F "SEntityRelativeMove"
    [ PF.entityID "entity"
      -- These should possibly use the same type as SEntityVelocity,
      -- but they're a different size, and represent a difference, not
      -- a velocity, so maybe not. (Even if the difference between the
      -- two is questionable, the entity velocity packets are relative
      -- to a time delta.)
    , PF.byte "dx"
    , PF.byte "dy"
    , PF.byte "dz"
    ]
  , packet 0x20 "SEntityLook"
    [ PF.entityID "entity"
      -- These are as a fraction of 360, apparently, so there are at
      -- least two formats for yaw and pitch. TODO: Figure out which
      -- one SNamedEntitySpawn uses.
    , PF.byte "yaw"
    , PF.byte "pitch"
    ]
  , packet 0x21 "SEntityLookRelativeMove"
    [ PF.entityID "entity"
      -- as above
    , PF.byte "dx"
    , PF.byte "dy"
    , PF.byte "dz"
      -- Same format as SEntityLook
    , PF.byte "yaw"
    , PF.byte "pitch"
    ]
  , packet 0x22 "SEntityTeleport"
    [ PF.entityID "entity"
    , PF.int "x"
    , PF.int "y"
    , PF.int "z"
      -- Same format as SEntityLook
    , PF.byte "yaw"
    , PF.byte "pitch"
    ]
  , packet 0x26 "SEntityStatus"
    [ PF.entityID "entity"
    , PF.byte "status" -- should have its own type
    ]
  , packet 0x27 "SAttachEntity"
    [ PF.entityID "attachedEntity"
      -- TODO FIXME: This is wrong! It is either deattaching, or an
      -- entity ID.
    , PF.entityID "vehicleEntity"
    ]
  , packet 0x28 "SEntityMetadata"
    [ PF.entityID "entity"
      -- as SMobSpawn; maybe rename this too?
    , PF.entityData "metadata"
    ]
  , packet 0x29 "SEntityEffect"
    [ PF.entityID "entity"
    , PF.byte "effect" -- should have its own type
    , PF.byte "amplifier"
    , PF.short "duration"
    ]
  , packet 0x2A "SRemoveEntityEffect"
    [ PF.entityID "entity"
    , PF.byte "effect" -- as above
    ]
  , packet 0x2B "SExperience"
    [ PF.byte "currentExperience"
    , PF.byte "experienceLevel"
    , PF.short "totalExperience"
    ]
  , packet 0x32 "SPreChunk"
    [ PF.int "x"
    , PF.int "z"
      -- This should have its own type. "If mode is 0 the client will
      -- unload the chunk, otherwise the client will initialize the
      -- chunk"
    , PF.bool "mode"
    ]
  , packet 0x33 "SMapChunk"
    [ PF.int "x"
    , PF.short "y"
    , PF.int "z"
      -- These are the real size minus 1.
    , PF.byte "xSize"
    , PF.byte "ySize"
    , PF.byte "zSize"
    , PF.mapChunk "data"
    ]
    -- This packet is completely awful.
  , packet 0x34 "SMultiBlockChange"
    [ PF.int "chunkX"
    , PF.int "chunkY"
    , PF.multiBlockChangeData "data"
    ]
  , packet 0x35 "SBlockChange"
    [ PF.int "x"
    , PF.byte "y"
    , PF.int "z"
    , PF.block "block"
    ]
  , packet 0x36 "SBlockAction"
    [ PF.int "x"
    , PF.short "y"
    , PF.int "z"
      -- This packet is horrible in every way; the fields are state
      -- and direction if this is a piston, but instrument and pitch
      -- if it's a note block. We can't just make this into a data
      -- type because its meaning depends on the state of the map.
    , PF.byte "data1"
    , PF.byte "data2"
    ]
  , packet 0x3C "SExplosion"
    [ PF.double "x"
    , PF.double "y"
    , PF.double "z"
    , PF.float "unknown" -- wiki says this might be radius
    , PF.explosionData "data"
    ]
  , packet 0x3D "SSoundEffect"
    [ PF.int "effect" -- as above
    , PF.int "x"
    , PF.byte "y"
    , PF.int "z"
      -- should be a custom type, but since it depends on the effect
      -- this whole packet will have to become a custom type; FIXME:
      -- is there a solution that isn't that ugly?
    , PF.int "data"
    ]
  , packet 0x46 "SNewOrInvalidState"
    [ PF.byte "reason" -- should have its own type
    , PF.bool "isCreative" -- should have its own type, as above
    ]
  , packet 0x47 "SThunderbolt"
    [ PF.entityID "entity"
    , PF.bool "unknown" -- always true
    , PF.int "x"
    , PF.int "y"
    , PF.int "z"
    ]
  , packet 0x64 "SOpenWindow"
    [ PF.windowID "window"
    , PF.byte "type" -- should have its own type
    , PF.string "title"
    , PF.byte "slots"
    ]
  , packet 0x65 "SCloseWindow"
    [ PF.windowID "window"
    ]
  , packet 0x67 "SSetSlot"
    [ PF.windowID "window" -- actually window ID or 0 for player inventory; this should have its own type
    , PF.short "slot"
    , PF.maybeHeldItem "item"
    ]
  , packet 0x68 "SWindowItems"
    [ PF.windowID "window" -- as above
    , PF.windowItems "items"
    ]
  , packet 0x6A "STransaction"
    [ PF.windowID "window" -- probably as above; not sure
    , PF.short "action" -- should have its own type, as (far) above
    , PF.bool "accepted"
    ]
  , packet 0x6B "SCreativeInventoryAction"
    [ PF.short "slot"
      -- These should really go into a type, but the amount and damage
      -- fields are always sent, even when the item is -1, unlike
      -- every other instance. Sigh.
    , PF.short "item"
    , PF.short "amount"
    , PF.short "metadata"
    ]
    -- Identical to the client version.
  , packet 0x82 "SUpdateSign"
    [ PF.int "x"
    , PF.short "y"
    , PF.int "z"
    , PF.string "line1"
    , PF.string "line2"
    , PF.string "line3"
    , PF.string "line4"
    ]
    -- Called "Item Data" on the wiki.
  , packet 0x83 "SMapData"
    [ PF.item "item"
    , PF.mapData "data"
    ]
  , packet 0xC8 "SIncrementStatistic"
    [ PF.int "statistic" -- should have its own type
    , PF.byte "amount"
    ]
  , packet 0xC9 "SPlayerListItem"
    [ PF.string "name"
    , PF.bool "online"
    , PF.short "ping" -- in ms
    ]
  , packet 0xFF "SKick"
    [ PF.string "reason"
    ]
  ]
