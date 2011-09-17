{-# LANGUAGE TemplateHaskell #-}

module MC.Protocol.Template
  ( PacketInfo
  , FieldInfo(..)
  , packet
  , packetType
  ) where

import Numeric
import Data.Word
import Data.Serialize (Serialize, Get)
import qualified Data.Serialize as SE
import Control.Applicative
import Control.Monad
import Language.Haskell.TH

data PacketInfo = Packet Word8 Name [FieldInfo]

data FieldInfo = FieldInfo
  { fieldType :: TypeQ
  , fieldName :: String
  , fieldGet  :: ExpQ
  , fieldPut  :: ExpQ
  }

packet :: Word8 -> String -> [FieldInfo] -> PacketInfo
packet ptype strName = Packet ptype (mkName strName)

packetType :: String -> [PacketInfo] -> Q [Dec]
packetType strName packets =
  sequence [ dataD (return []) name [] (map packetCon packets) [''Eq, ''Show]
           , instanceD (return []) (appT (conT ''Serialize) (conT name))
             [ funD 'SE.get [clause [] (normalB (getExp packets)) []]
             , funD 'SE.put $ map putClause packets
             ]
           ]
  where name = mkName strName
        packetCon (Packet _ pname fields) = normalC pname $ map packetField fields
        packetField fi = (,) IsStrict <$> fieldType fi
        getExp xs = do
          typeVar <- newName "t"
          doE [ bindS (varP typeVar) $ varE 'SE.getWord8
              , noBindS . caseE (varE typeVar) $
                  map getClause xs ++
                  [ match wildP (normalB (appE (varE 'unknownPacketType) (varE typeVar))) [] ]
              ]
          where getClause (Packet ptype pname fields) = match ptypeP (normalB getClauseExp) []
                  where ptypeP = litP (integerL (fromIntegral ptype))
                        getClauseExp = foldl apE (appE (varE 'pure) (conE pname)) . map fieldGet $ fields
                        apE f x = infixE (Just f) (varE '(<*>)) (Just x)
        putClause (Packet ptype pname fields) = do
          putFields <- forM fields $ \fi -> flip (,) (fieldPut fi) <$> newName (fieldName fi)
          clause [conP pname (map (varP . fst) putFields)] (normalB (putExp putFields)) []
          where ptypeE = litE (integerL (fromIntegral ptype))
                putExp xs = doE $
                 noBindS (appE (varE 'SE.putWord8) ptypeE) :
                 map (\(fname,put) -> noBindS $ appE put (varE fname)) xs

unknownPacketType :: Word8 -> Get a
unknownPacketType t = fail $ "Unknown packet type 0x" ++ showHex t ""
