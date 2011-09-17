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
  , itemID
  , windowID
  , heldItem
  , placement
  ) where

import MC.Protocol.Types
import MC.Protocol.Template

import Data.Int
import Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Serialize (Get, Putter)
import qualified Data.Serialize as SE
import Control.Applicative
import Language.Haskell.TH

getTextUCS2be :: Get Text
getTextUCS2be = do
  len <- fromIntegral <$> SE.getWord64be
  TE.decodeUtf16BEWith TEE.ignore <$> SE.getBytes len

putTextUCS2be :: Putter Text
putTextUCS2be text = do
  SE.putWord64be . fromIntegral $ T.length text
  SE.putByteString $ TE.encodeUtf16BE text

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
float = simpleField [t| Float |]

double :: String -> FieldInfo
double = simpleField [t| Double |]

string :: String -> FieldInfo
string name = FieldInfo
  { fieldType = [t| Text |]
  , fieldName = name
  , fieldGet  = [| getTextUCS2be |]
  , fieldPut  = [| putTextUCS2be |]
  }

bool :: String -> FieldInfo
bool = simpleField [t| Bool |]

entityID :: String -> FieldInfo
entityID = simpleField [t| EntityID |]

itemID :: String -> FieldInfo
itemID = simpleField [t| ItemID |]

windowID :: String -> FieldInfo
windowID = simpleField [t| WindowID |]

heldItem :: String -> FieldInfo
heldItem = simpleField [t| HeldItem |]

placement :: String -> FieldInfo
placement = simpleField [t| Placement |]
