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
  , heldItem
  , placement
  ) where

import MC.Protocol.Types
import MC.Protocol.Template

import Data.Int
import Data.Word
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Serialize (Get, Putter)
import qualified Data.Serialize as SE
import qualified Data.Serialize.IEEE754 as SE754
import Control.Applicative
import Language.Haskell.TH

getTextUTF16be :: Get Text
getTextUTF16be = do
  len <- SE.get :: Get Word16
  TE.decodeUtf16BEWith TEE.ignore <$> SE.getBytes (fromIntegral len * 2)

putTextUTF16be :: Putter Text
putTextUTF16be text = do
  -- The length is sent as the number of UTF-16 components, not as the
  -- number of codepoints; surrogates are counted as two
  -- components. Data.Text.length returns the number of codepoints, so
  -- it's not suitable here.
  let encoded = TE.encodeUtf16BE text
  SE.put (fromIntegral (B.length encoded `div` 2) :: Word16)
  SE.putByteString encoded

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

heldItem :: String -> FieldInfo
heldItem = simpleField [t| HeldItem |]

placement :: String -> FieldInfo
placement = simpleField [t| Placement |]
