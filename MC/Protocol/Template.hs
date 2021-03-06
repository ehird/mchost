{-# LANGUAGE TemplateHaskell, MagicHash #-}

module MC.Protocol.Template
  ( PacketInfo
  , FieldInfo(..)
  , packet
  , packetType
  ) where

import MC.Protocol.Types

import Numeric
import Data.Word
import Data.Serialize (Serialize, Get)
import qualified Data.Serialize as SE
import Control.Applicative
import Language.Haskell.TH
import GHC.Word (Word8(..))

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
           , instanceD (return []) (appT (conT ''Packet) (conT name))
             [ funD 'packetName $ map nameClause packets
             , funD 'packetShowsFieldsPrec $ map fieldsClause packets
             ]
           , instanceD (return []) (appT (conT ''Serialize) (conT name))
             [ funD 'SE.get [clause [] (normalB (getExp packets)) []]
             , funD 'SE.put $ map putClause packets
             ]
           ]
  where name = mkName strName
        packetCon (Packet _ pname fields) = recC pname $ map (packetField pname) fields
        packetField pname fi = (,,) (mkFieldName pname (fieldName fi)) IsStrict <$> fieldType fi
        mkFieldName pname fname = mkName $ prefixFieldName (nameBase pname) fname
        nameClause (Packet _ pname _) = clause [recP pname []] (normalB (litE (stringL (nameBase pname)))) []
        -- empty fields case to silence warnings about "prec" being unused
        fieldsClause (Packet _ pname []) = clause [wildP, conP pname []] (normalB (listE [])) []
        fieldsClause (Packet _ pname fields) = do
          precVar <- newName "prec"
          vars <- mapM (newName . fieldName) fields
          let nameLits = map (stringL . fieldName) fields
          clause [varP precVar, conP pname (map varP vars)]
            (normalB (listE [ tupE [litE nameLit, [| showsPrec $(varE precVar) $(varE var) |] ] | (var,nameLit) <- zip vars nameLits ])) []
        getExp xs = do
          typeVar <- newName "t"
          unboxedTypeVar <- newName "t#"
          doE [ bindS (asP typeVar (conP 'W8# [varP unboxedTypeVar])) $ varE 'SE.getWord8
              , noBindS . caseE (varE unboxedTypeVar) $
                  map getClause xs ++
                  [ match wildP (normalB (appE (varE 'unknownPacketType) (varE typeVar))) [] ]
              ]
          where getClause (Packet ptype pname fields) = match ptypeP (normalB getClauseExp) []
                  where ptypeP = litP (wordPrimL (fromIntegral ptype))
                        getClauseExp = do
                          vars <- mapM (newName . fieldName) fields
                          let gets = map fieldGet fields
                          doE $
                            [ bindS (varP var) get | (var,get) <- zip vars gets ] ++
                            [ noBindS $ appE (varE 'return) (foldl appE (conE pname) (map varE vars)) ]
        putClause (Packet ptype pname fields) = do
          vars <- mapM (newName . fieldName) fields
          let puts = map fieldPut fields
          clause [conP pname (map varP vars)] (normalB (putExp vars puts)) []
          where ptypeE = litE (integerL (fromIntegral ptype))
                putExp vars puts = doE $
                 noBindS (appE (varE 'SE.putWord8) ptypeE) :
                 [ noBindS $ appE put (varE var) | (var,put) <- zip vars puts ]

unknownPacketType :: Word8 -> Get a
unknownPacketType t = fail $ "Unknown packet type 0x" ++ showHex t ""
