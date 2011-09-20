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
  , getMaybeHeldItem
  , putMaybeHeldItem
  , Block(..)
  , Placement(..)
  , Equipment(..)
  , CurrentItem(..)
  , Fireball(..)
  , ExplosionData(..)
  , ExplosionItem(..)
  , WindowItems(..)
  , MultiBlockChangeData(..)
  , MultiBlockChangeItem(..)
  , MapData(..)
  , EntityData(..)
  , EntityField(..)
  , EntityFieldValue(..)
  , ServerHandshake(..)
  ) where

import Data.Int
import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Serialize (Serialize, Get, Putter)
import qualified Data.Serialize as SE
import qualified Data.Serialize.IEEE754 as SE754
import Control.Applicative
import Control.Monad

getTextUTF16be :: Get Text
getTextUTF16be = do
  len <- SE.get :: Get Int16
  TE.decodeUtf16BEWith TEE.ignore <$> SE.getBytes (fromIntegral len * 2)

putTextUTF16be :: Putter Text
putTextUTF16be text = do
  -- The length is sent as the number of UTF-16 components, not as the
  -- number of codepoints; surrogates are counted as two
  -- components. Data.Text.length returns the number of codepoints, so
  -- it's not suitable here.
  let encoded = TE.encodeUtf16BE text
  SE.put (fromIntegral (B.length encoded `div` 2) :: Int16)
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

getMaybeHeldItem :: Get (Maybe HeldItem)
getMaybeHeldItem = do
  let getShort = SE.get :: Get Int16
  sh <- SE.lookAhead getShort
  if sh < 0
    then getShort >> return Nothing
    else Just <$> SE.get

putMaybeHeldItem :: Putter (Maybe HeldItem)
putMaybeHeldItem Nothing = SE.put (-1 :: Int16)
putMaybeHeldItem (Just heldItem) = SE.put heldItem

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
      then getShort >> getShort >> return NothingEquipped
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
    if sh == 0
      then getShort >> return NoCurrentItem
      else CurrentItem <$> SE.get
  put NoCurrentItem = SE.put (0 :: Int16)
  put (CurrentItem itemID) = SE.put itemID

-- the EntityID is the entity ID of the fireball thrower, not of the
-- fireball itself
data Fireball = NotFireball | Fireball !EntityID !Int16 !Int16 !Int16 deriving (Eq, Show)

instance Serialize Fireball where
  get = do
    let getInt = SE.get :: Get Int32
    int <- SE.lookAhead getInt
    if int == 0
      then getInt >> return NotFireball
      else Fireball <$> SE.get <*> SE.get <*> SE.get <*> SE.get
  put NotFireball = SE.put (0 :: Int32)
  put (Fireball entityID unknown1 unknown2 unknown3) = do
    SE.put entityID
    SE.put unknown1
    SE.put unknown2
    SE.put unknown3

newtype ExplosionData = ExplosionData [ExplosionItem] deriving (Eq, Show)

-- FIXME: Are these signed or unsigned?
data ExplosionItem = ExplosionItem !Int8 !Int8 !Int8 deriving (Eq, Show)
 
instance Serialize ExplosionItem where
  get = ExplosionItem <$> SE.get <*> SE.get <*> SE.get
  put (ExplosionItem x y z) = do
    SE.put x
    SE.put y
    SE.put z

instance Serialize ExplosionData where
  get = do
    count <- SE.get :: Get Int16
    ExplosionData <$> replicateM (fromIntegral count) SE.get
  put (ExplosionData xs) = do
    SE.put (fromIntegral (length xs) :: Int16)
    mapM_ SE.put xs

newtype WindowItems = WindowItems [Maybe HeldItem] deriving (Eq, Show)
 
instance Serialize WindowItems where
  get = do
    count <- SE.get :: Get Int16
    WindowItems <$> replicateM (fromIntegral count) getMaybeHeldItem
  put (WindowItems xs) = do
    SE.put (fromIntegral (length xs) :: Int16)
    mapM_ putMaybeHeldItem xs

newtype MultiBlockChangeData = MultiBlockChangeData [MultiBlockChangeItem] deriving (Eq, Show)

-- FIXME: Should probably be named BlockChange and be used for single
-- block changes too
--
-- FIXME: The Int8 fields should possibly be Word8s
data MultiBlockChangeItem = MultiBlockChangeItem !Int8 !Int8 !Int8 !Block deriving (Eq, Show)

instance Serialize MultiBlockChangeData where
  get = do
    count <- fromIntegral <$> (SE.get :: Get Int16)
    coords <- replicateM count (unpackCoords <$> SE.get)
    types <- replicateM count SE.get
    metadata <- replicateM count SE.get
    return $ MultiBlockChangeData (zipWith3 makeItem coords types metadata)
    where -- FIXME: might be Word16
          unpackCoords :: Int16 -> (Int8,Int8,Int8)
          unpackCoords sh = (fromIntegral (sh `shiftL` 12), fromIntegral ((sh `shiftL` 8) .&. 0xF), fromIntegral (sh .&. 0xF))
          makeItem :: (Int8,Int8,Int8) -> Int8 -> Int8 -> MultiBlockChangeItem
          makeItem (x,y,z) blockType metadata = MultiBlockChangeItem x y z (Block (BlockID blockType) metadata)
  put (MultiBlockChangeData xs) = do
    SE.put (fromIntegral (length xs) :: Int16)
    mapM_ putCoords xs
    mapM_ putType xs
    mapM_ putMetadata xs
    where putCoords (MultiBlockChangeItem x y z _) =
            SE.put (fromIntegral (x `shiftR` 12) .|. fromIntegral (y `shiftR` 8) .|. fromIntegral z :: Int16)
          putType (MultiBlockChangeItem _ _ _ (Block (BlockID blockID) _)) = SE.put blockID
          putMetadata (MultiBlockChangeItem _ _ _ (Block _ metadata)) = SE.put metadata

-- TODO FIXME: Parse this properly rather than this ridiculously lazy hack
newtype MapData = MapData ByteString deriving (Eq, Show)

instance Serialize MapData where
  get = do
    bytes <- SE.getWord8
    MapData <$> SE.getByteString (fromIntegral bytes)
  put (MapData str) = do
    SE.putWord8 $ fromIntegral (B.length str)
    SE.putByteString str

newtype EntityData = EntityData [EntityField] deriving (Eq, Show)

-- FIXME: Should maybe be Int8
data EntityField = EntityField !Word8 !EntityFieldValue deriving (Eq, Show)

data EntityFieldValue
  = EntityInt8 !Int8
  | EntityInt16 !Int16
  | EntityInt32 !Int32
  | EntityFloat !Float
  | EntityString !Text
  | EntityHeldItem !HeldItem
  | EntityVector !Int32 !Int32 !Int32
  deriving (Eq, Show)

instance Serialize EntityField where
  get = do
    (fieldType,ident) <- unpackEntityByte <$> SE.getWord8
    value <- case fieldType of
      0 -> EntityInt8 <$> SE.get
      1 -> EntityInt16 <$> SE.get
      2 -> EntityInt32 <$> SE.get
      3 -> EntityFloat <$> SE754.getFloat32be
      4 -> EntityString <$> getTextUTF16be
      5 -> EntityHeldItem <$> SE.get
      6 -> EntityVector <$> SE.get <*> SE.get <*> SE.get
      _ -> fail $ "Unknown entity data field type: " ++ show fieldType
    return (EntityField ident value)
    where unpackEntityByte byte = (byte `shiftR` 5, byte .&. 0x1F)
  put (EntityField ident value) = do
    let (fieldType,action) = f value
    SE.putWord8 $ (fieldType `shiftL` 5) .|. ident
    action
    where f (EntityInt8 v) = (0, SE.put v)
          f (EntityInt16 v) = (1, SE.put v)
          f (EntityInt32 v) = (2, SE.put v)
          f (EntityFloat v) = (3, SE754.putFloat32be v)
          f (EntityString s) = (4, putTextUTF16be s)
          f (EntityHeldItem heldItem) = (5, SE.put heldItem)
          f (EntityVector x y z) = (6, SE.put x >> SE.put y >> SE.put z)

instance Serialize EntityData where
  get = do
    byte <- SE.lookAhead SE.getWord8
    if byte == 0x7F
      then SE.getWord8 >> return (EntityData [])
      else do
        field <- SE.get
        EntityData rest <- SE.get
        return (EntityData (field:rest))
  put (EntityData xs) = do
    mapM_ SE.put xs
    SE.putWord8 0x7F

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
