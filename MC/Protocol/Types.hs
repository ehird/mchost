{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MC.Protocol.Types
  ( EntityID(..)
  , getEntityID
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
import Data.Serialize (Serialize, Get)
import qualified Data.Serialize as SE
import Control.Applicative

newtype EntityID = EntityID Int32 deriving (Eq, Show, Serialize)

getEntityID :: EntityID -> Int32
getEntityID (EntityID i) = i

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
