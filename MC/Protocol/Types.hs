{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MC.Protocol.Types
  ( getTextUTF16be
  , putTextUTF16be
  , EntityID(..)
  , getEntityID
  , WorldID(..)
  , getWorldID
  , BlockID(..)
  , getBlockID
  , ItemID(..)
  , getItemID
  , WindowID(..)
  , getWindowID
  , ItemOrBlockID(..)
  , HeldItem(..)
  , Placement(..)
  ) where

import Data.Int
import Data.Word
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Serialize (Serialize, Get, Putter)
import qualified Data.Serialize as SE
import Control.Applicative

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

newtype EntityID = EntityID Int32 deriving (Eq, Show, Serialize)

getEntityID :: EntityID -> Int32
getEntityID (EntityID i) = i

newtype WorldID = WorldID Int8 deriving (Eq, Show, Serialize)

getWorldID :: WorldID -> Int8
getWorldID (WorldID i) = i

newtype BlockID = BlockID Int8 deriving (Eq, Show, Serialize)

getBlockID :: BlockID -> Int8
getBlockID (BlockID i) = i

newtype ItemID = ItemID Int16 deriving (Eq, Show, Serialize)

getItemID :: ItemID -> Int16
getItemID (ItemID i) = i

newtype WindowID = WindowID Int8 deriving (Eq, Show, Serialize)

getWindowID :: WindowID -> Int8
getWindowID (WindowID i) = i

data ItemOrBlockID
  = IsItem ItemID
  | IsBlock BlockID
  deriving (Eq, Show)

instance Serialize ItemOrBlockID where
  get = do
    sh <- SE.lookAhead SE.get :: Get Int16
    if sh > 255
      then IsItem <$> SE.get
      else IsBlock <$> SE.get
  put (IsItem itemID) = SE.put itemID
  put (IsBlock blockID) = SE.put blockID

data HeldItem = HeldItem
  { heldItemOrBlockID :: ItemOrBlockID
  , heldAmount :: Word8
  , heldDamage :: Word16
  } deriving (Eq, Show)

instance Serialize HeldItem where
  get = HeldItem <$> SE.get <*> SE.getWord8 <*> SE.get
  put (HeldItem itemOrBlockID amount damage) = do
    SE.put itemOrBlockID
    SE.putWord8 amount
    SE.put damage

data Placement = EmptyHanded | Place HeldItem deriving (Eq, Show)

instance Serialize Placement where
  get = do
    let getShort = SE.get :: Get Int16
    sh <- SE.lookAhead getShort
    if sh < 0
      then getShort >> return EmptyHanded
      else Place <$> SE.get
  put EmptyHanded = SE.put (-1 :: Int16)
  put (Place heldItem) = SE.put heldItem
