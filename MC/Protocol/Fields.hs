{-# LANGUAGE TemplateHaskell #-}

module MC.Protocol.Fields
  ( byte
  , ubyte
  , short
  , int
  , long
  , float
  , double
  , string
  , bool
  , entityID
  , worldID
  , windowID
  , item
  , heldItem
  , block
  , placement
  , equipment
  , currentItem
  , fireball
  , explosionData
  , windowItems
  , multiBlockChangeData
  , mapData
  , entityData
  , serverHandshake
  , lengthPrefixedByteString
  ) where

import MC.Protocol.Types
import MC.Protocol.Template

import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Serialize as SE
import qualified Data.Serialize.IEEE754 as SE754
import Language.Haskell.TH

simpleField :: TypeQ -> String -> FieldInfo
simpleField typeQ name = FieldInfo
  { fieldType = typeQ
  , fieldName = name
  , fieldGet  = [| SE.get |]
  , fieldPut  = [| SE.put |]
  }

byte :: String -> FieldInfo
byte = simpleField [t| Int8 |]

ubyte :: String -> FieldInfo
ubyte name = FieldInfo
  { fieldType = [t| Word8 |]
  , fieldName = name
  , fieldGet  = [| SE.getWord8 |]
  , fieldPut  = [| SE.putWord8 |]
  }

short :: String -> FieldInfo
short = simpleField [t| Int16 |]

int :: String -> FieldInfo
int = simpleField [t| Int32 |]

long :: String -> FieldInfo
long = simpleField [t| Int64 |]

float :: String -> FieldInfo
float name = FieldInfo
  { fieldType = [t| Float |]
  , fieldName = name
  , fieldGet  = [| SE754.getFloat32be |]
  , fieldPut  = [| SE754.putFloat32be |]
  }

double :: String -> FieldInfo
double name = FieldInfo
  { fieldType = [t| Double |]
  , fieldName = name
  , fieldGet  = [| SE754.getFloat64be |]
  , fieldPut  = [| SE754.putFloat64be |]
  }

string :: String -> FieldInfo
string name = FieldInfo
  { fieldType = [t| Text |]
  , fieldName = name
  , fieldGet  = [| getTextUTF16be |]
  , fieldPut  = [| putTextUTF16be |]
  }

bool :: String -> FieldInfo
bool = simpleField [t| Bool |]

entityID :: String -> FieldInfo
entityID = simpleField [t| EntityID |]

worldID :: String -> FieldInfo
worldID = simpleField [t| WorldID |]

windowID :: String -> FieldInfo
windowID = simpleField [t| WindowID |]

item :: String -> FieldInfo
item = simpleField [t| Item |]

heldItem :: String -> FieldInfo
heldItem = simpleField [t| HeldItem |]

block :: String -> FieldInfo
block = simpleField [t| Block |]

placement :: String -> FieldInfo
placement = simpleField [t| Placement |]

equipment :: String -> FieldInfo
equipment = simpleField [t| Equipment |]

currentItem :: String -> FieldInfo
currentItem = simpleField [t| CurrentItem |]

fireball :: String -> FieldInfo
fireball = simpleField [t| Fireball |]

explosionData :: String -> FieldInfo
explosionData = simpleField [t| ExplosionData |]

windowItems :: String -> FieldInfo
windowItems = simpleField [t| WindowItems |]

multiBlockChangeData :: String -> FieldInfo
multiBlockChangeData = simpleField [t| MultiBlockChangeData |]

mapData :: String -> FieldInfo
mapData = simpleField [t| MapData |]

entityData :: String -> FieldInfo
entityData = simpleField [t| EntityData |]

serverHandshake :: String -> FieldInfo
serverHandshake = simpleField [t| ServerHandshake |]

lengthPrefixedByteString :: String -> FieldInfo
lengthPrefixedByteString name = FieldInfo
  { fieldType = [t| ByteString |]
  , fieldName = name
  , fieldGet  = [| getLengthPrefixedByteString |]
  , fieldPut  = [| putLengthPrefixedByteString |]
  }
