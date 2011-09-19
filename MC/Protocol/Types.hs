{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module MC.Protocol.Types
  ( getTextUTF16be
  , putTextUTF16be
  , getLengthPrefixedByteString
  , putLengthPrefixedByteString
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
  , Item(..)
  , HeldItem(..)
  , Block(..)
  , Placement(..)
  , Equipment(..)
  , CurrentItem(..)
  , ServerHandshake(..)
  ) where

import Data.Int
import Data.Word
import Data.ByteString (ByteString)
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

getLengthPrefixedByteString :: Get ByteString
getLengthPrefixedByteString = do
  bytes <- SE.getWord8
  SE.getByteString (fromIntegral bytes)

putLengthPrefixedByteString :: Putter ByteString
putLengthPrefixedByteString str = do
  SE.putWord8 $ fromIntegral (B.length str)
  SE.putByteString str

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
  = IsItem !ItemID
  | IsBlock !BlockID
  deriving (Eq, Show)

instance Serialize ItemOrBlockID where
  get = do
    sh <- SE.lookAhead SE.get :: Get Int16
    if sh > 255
      then IsItem <$> SE.get
      else IsBlock <$> SE.get
  put (IsItem itemID) = SE.put itemID
  put (IsBlock blockID) = SE.put blockID

-- Int16 is metadata
data Item = Item !ItemOrBlockID !Int16 deriving (Eq, Show)

-- Note that this instance is *not* used in other instances; usually
-- the amount appears first.
instance Serialize Item where
  get = Item <$> SE.get <*> SE.get
  put (Item itemOrBlockID metadata) = do
    SE.put itemOrBlockID
    SE.put metadata

-- Int8 is amount
data HeldItem = HeldItem !Item !Int8 deriving (Eq, Show)

instance Serialize HeldItem where
  get = do
    itemOrBlockID <- SE.get
    amount <- SE.get
    metadata <- SE.get
    return $ HeldItem (Item itemOrBlockID metadata) amount
  put (HeldItem (Item itemOrBlockID metadata) amount) = do
    SE.put itemOrBlockID
    SE.put amount
    SE.put metadata

-- Int8 is metadata
data Block = Block !BlockID !Int8 deriving (Eq, Show)

instance Serialize Block where
  get = Block <$> SE.get <*> SE.get
  put (Block blockID metadata) = do
    SE.put blockID
    SE.put metadata

data Placement = EmptyHanded | Place !HeldItem deriving (Eq, Show)

instance Serialize Placement where
  get = do
    let getShort = SE.get :: Get Int16
    sh <- SE.lookAhead getShort
    if sh < 0
      then getShort >> return EmptyHanded
      else Place <$> SE.get
  put EmptyHanded = SE.put (-1 :: Int16)
  put (Place heldItem) = SE.put heldItem

-- FIXME: Annoying duplication with HeldItem and Placement, especially
-- since -1 is used to denote "nothing held" in both

data Equipment = NothingEquipped | Equipped !Item deriving (Eq, Show)

instance Serialize Equipment where
  get = do
    let getShort = SE.get :: Get Int16
    sh <- SE.lookAhead getShort
    if sh < 0
      then getShort >> return NothingEquipped
      else Equipped <$> (Item <$> SE.get <*> SE.get)
  put NothingEquipped = do
    SE.put (-1 :: Int16)
    -- FIXME: What does the official server send for metadata in this
    -- case?
    SE.put (0 :: Int16)
  put (Equipped (Item itemOrBlockID metadata)) = do
    SE.put itemOrBlockID
    SE.put metadata

data CurrentItem = NoCurrentItem | CurrentItem !ItemID deriving (Eq, Show)

instance Serialize CurrentItem where
  get = do
    let getShort = SE.get :: Get Int16
    sh <- SE.lookAhead getShort
    if sh < 0
      then getShort >> return NoCurrentItem
      else CurrentItem <$> SE.get
  put NoCurrentItem = SE.put (0 :: Int16)
  put (CurrentItem itemID) = SE.put itemID

data ServerHandshake
  = NoAuthentication
  | Authenticate
  -- The field is a unique connection ID; the official server
  -- generates a 64-bit word and converts it to lowercase hexadecimal
  -- to generate this.
  --
  -- FIXME: Maybe it should be a String instead? It's pretty short,
  -- though so are all the strings used in the protocol, and Text is
  -- used for them. It'd still have to be converted to Text to
  -- serialise it, because there's no UTF-16 decoder for Strings.
  | LoggedIn !Text
  deriving (Eq, Show)

instance Serialize ServerHandshake where
  get = do
    str <- getTextUTF16be
    case str of
      "-" -> return NoAuthentication
      "+" -> return Authenticate
      _   -> return (LoggedIn str)
  put NoAuthentication = putTextUTF16be "-"
  put Authenticate = putTextUTF16be "+"
  put (LoggedIn str) = putTextUTF16be str
