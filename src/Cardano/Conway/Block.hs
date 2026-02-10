{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use <$>" -}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Conway.Block where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Word as Word
import qualified Data.Map.Strict as Map
import           Data.Int (Int64)
import           Codec.CBOR.Decoding
                   ( Decoder, TokenType(..)
                   , decodeListLen, decodeListLenOrIndef
                   , decodeBytes, decodeString
                   , decodeWord64, decodeWord32, decodeWord16, decodeWord, decodeWord8
                   , decodeMapLen, decodeMapLenOrIndef
                   , decodeNull, decodeTag, decodeInteger, decodeInt64
                   , decodeBool, decodeBreakOr
                   , peekTokenType
                   )
import           Control.Monad (replicateM, replicateM_, void, when)

--------------------------------------------------------------------------------
-- Primitive newtypes
--------------------------------------------------------------------------------

newtype Hash32 = Hash32 BS.ByteString
  deriving (Show, Eq, Ord)

decodeHash32 :: Decoder s Hash32
decodeHash32 = Hash32 <$> decodeBytes

newtype Hash28 = Hash28 BS.ByteString
  deriving (Show, Eq, Ord)

decodeHash28 :: Decoder s Hash28
decodeHash28 = Hash28 <$> decodeBytes

newtype VKey = VKey BS.ByteString
  deriving (Show, Eq)

decodeVKey :: Decoder s VKey
decodeVKey = VKey <$> decodeBytes

newtype VrfVKey = VrfVKey BS.ByteString
  deriving (Show, Eq)

decodeVrfVKey :: Decoder s VrfVKey
decodeVrfVKey = VrfVKey <$> decodeBytes

type KesSignature = BS.ByteString
type TransactionIndex = Word.Word16

--------------------------------------------------------------------------------
-- VRF certificate
--------------------------------------------------------------------------------

data VrfCert = VrfCert BS.ByteString BS.ByteString
  deriving (Show, Eq)

decodeVrfCert :: Decoder s VrfCert
decodeVrfCert = do
  _ <- decodeListLen
  v1 <- decodeBytes
  v2 <- decodeBytes
  return $ VrfCert v1 v2

--------------------------------------------------------------------------------
-- Header types
--------------------------------------------------------------------------------

data OperationalCert = OperationalCert
  { opCertHotVKey   :: VKey
  , opCertSeqNum    :: Word.Word64
  , opCertKesPeriod :: Word.Word64
  , opCertSigma     :: BS.ByteString
  } deriving (Show, Eq)

decodeOperationalCert :: Decoder s OperationalCert
decodeOperationalCert = do
  _ <- decodeListLen
  hotVKey   <- decodeVKey
  seqNum    <- decodeWord64
  kesPeriod <- decodeWord64
  sigma     <- decodeBytes
  return $ OperationalCert hotVKey seqNum kesPeriod sigma

data ProtocolVersion = ProtocolVersion
  { pvMajor :: Word.Word64
  , pvMinor :: Word.Word64
  } deriving (Show, Eq)

decodeProtocolVersion :: Decoder s ProtocolVersion
decodeProtocolVersion = do
  _ <- decodeListLen
  major <- decodeWord64
  minor <- decodeWord64
  return $ ProtocolVersion major minor

data HeaderBody = HeaderBody
  { hbBlockNumber    :: Word.Word64
  , hbSlot           :: Word.Word64
  , hbPrevHash       :: Maybe Hash32
  , hbIssuerVKey     :: VKey
  , hbVrfVKey        :: VrfVKey
  , hbVrfResult      :: VrfCert
  , hbBlockBodySize  :: Word.Word32
  , hbBlockBodyHash  :: Hash32
  , hbOperationalCert :: OperationalCert
  , hbProtocolVersion :: ProtocolVersion
  } deriving (Show, Eq)

decodeHeaderBody :: Decoder s HeaderBody
decodeHeaderBody = do
  _ <- decodeListLen
  blockNumber     <- decodeWord64
  slot            <- decodeWord64
  prevHash        <- decodeMaybe decodeHash32
  issuerVKey      <- decodeVKey
  vrfVKey         <- decodeVrfVKey
  vrfResult       <- decodeVrfCert
  blockBodySize   <- decodeWord32
  blockBodyHash   <- decodeHash32
  operationalCert <- decodeOperationalCert
  protocolVersion <- decodeProtocolVersion
  return $ HeaderBody blockNumber slot prevHash issuerVKey vrfVKey
                      vrfResult blockBodySize blockBodyHash
                      operationalCert protocolVersion

data Header = Header
  { hHeaderBody    :: HeaderBody
  , hBodySignature :: KesSignature
  } deriving (Show, Eq)

decodeHeader :: Decoder s Header
decodeHeader = do
  _ <- decodeListLen
  headerBody    <- decodeHeaderBody
  bodySignature <- decodeBytes
  return $ Header headerBody bodySignature

--------------------------------------------------------------------------------
-- Shared decoders
--------------------------------------------------------------------------------

-- | Decode a CBOR nullable: null → Nothing, otherwise Just a
decodeMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeMaybe decoder = do
  tt <- peekTokenType
  case tt of
    TypeNull -> do
      decodeNull
      return Nothing
    _ -> Just <$> decoder

-- | Decode a CBOR list into a Haskell list
decodeListOf :: Decoder s a -> Decoder s [a]
decodeListOf decoder = do
  mLen <- decodeListLenOrIndef
  case mLen of
    Just len -> replicateM len decoder
    Nothing  -> decodeIndefList decoder

decodeIndefList :: Decoder s a -> Decoder s [a]
decodeIndefList decoder = go []
  where
    go acc = do
      done <- decodeBreakOr
      if done
        then return (reverse acc)
        else do
          x <- decoder
          go (x : acc)

-- | Decode a CBOR map into a Haskell Map
decodeMapOf :: Ord k => Decoder s k -> Decoder s v -> Decoder s (Map.Map k v)
decodeMapOf decK decV = do
  mLen <- decodeMapLenOrIndef
  case mLen of
    Just len -> fmap Map.fromList $ replicateM len decodePair
    Nothing  -> fmap Map.fromList $ decodeIndefList decodePair
  where
    decodePair = do
      k <- decK
      v <- decV
      return (k, v)

-- | Decode a set: #6.258([* a]) / [* a]
--   If there's a tag 258, consume it, then decode the list.
decodeSet :: Decoder s a -> Decoder s [a]
decodeSet decoder = do
  tt <- peekTokenType
  case tt of
    TypeTag -> do
      tag <- decodeTag
      when (tag /= 258) $ fail $ "Expected tag 258 for set, got " ++ show tag
      decodeListOf decoder
    _ -> decodeListOf decoder

-- | Decode a non-empty set (same encoding, just semantically non-empty)
decodeNonemptySet :: Decoder s a -> Decoder s [a]
decodeNonemptySet = decodeSet

--------------------------------------------------------------------------------
-- Credential / DRep / Anchor
--------------------------------------------------------------------------------

data Credential
  = CredKeyHash Hash28
  | CredScriptHash Hash28
  deriving (Show, Eq, Ord)

decodeCredential :: Decoder s Credential
decodeCredential = do
  _ <- decodeListLen
  tag <- decodeWord
  case tag of
    0 -> CredKeyHash <$> decodeHash28
    1 -> CredScriptHash <$> decodeHash28
    _ -> fail $ "Unknown credential tag: " ++ show tag

data DRep
  = DRepKeyHash Hash28
  | DRepScriptHash Hash28
  | DRepAlwaysAbstain
  | DRepAlwaysNoConfidence
  deriving (Show, Eq, Ord)

decodeDRep :: Decoder s DRep
decodeDRep = do
  _ <- decodeListLen
  tag <- decodeWord
  case tag of
    0 -> DRepKeyHash <$> decodeHash28
    1 -> DRepScriptHash <$> decodeHash28
    2 -> return DRepAlwaysAbstain
    3 -> return DRepAlwaysNoConfidence
    _ -> fail $ "Unknown DRep tag: " ++ show tag

data Anchor = Anchor
  { anchorUrl      :: T.Text
  , anchorDataHash :: Hash32
  } deriving (Show, Eq)

decodeAnchor :: Decoder s Anchor
decodeAnchor = do
  _ <- decodeListLen
  url      <- decodeString
  dataHash <- decodeHash32
  return $ Anchor url dataHash

--------------------------------------------------------------------------------
-- Transaction Input
--------------------------------------------------------------------------------

data TransactionInput = TransactionInput
  { txInputId    :: Hash32
  , txInputIndex :: Word.Word16
  } deriving (Show, Eq, Ord)

decodeTransactionInput :: Decoder s TransactionInput
decodeTransactionInput = do
  _ <- decodeListLen
  txId  <- decodeHash32
  index <- decodeWord16
  return $ TransactionInput txId index

--------------------------------------------------------------------------------
-- Value and MultiAsset
--------------------------------------------------------------------------------

type PolicyId = Hash28
type AssetName = BS.ByteString

data Value
  = ValueLovelace Word.Word64
  | ValueMultiAsset Word.Word64 (Map.Map PolicyId (Map.Map AssetName Integer))
  deriving (Show, Eq)

decodeValue :: Decoder s Value
decodeValue = do
  tt <- peekTokenType
  case tt of
    TypeUInt   -> ValueLovelace <$> decodeWord64
    TypeUInt64 -> ValueLovelace <$> decodeWord64
    _          -> do
      _ <- decodeListLen
      coin   <- decodeWord64
      assets <- decodeMultiAsset decodeInteger
      return $ ValueMultiAsset coin assets

decodeMultiAsset :: Decoder s a -> Decoder s (Map.Map PolicyId (Map.Map AssetName a))
decodeMultiAsset decodeAmount = decodeMapOf decodeHash28 (decodeMapOf decodeBytes decodeAmount)

decodePositiveValue :: Decoder s Value
decodePositiveValue = do
  tt <- peekTokenType
  case tt of
    TypeUInt   -> ValueLovelace <$> decodeWord64
    TypeUInt64 -> ValueLovelace <$> decodeWord64
    _          -> do
      _ <- decodeListLen
      coin   <- decodeWord64
      assets <- decodeMultiAsset decodeWord64AsInteger
      return $ ValueMultiAsset coin assets
  where
    decodeWord64AsInteger = fromIntegral <$> decodeWord64

--------------------------------------------------------------------------------
-- Transaction Output
--------------------------------------------------------------------------------

data DatumOption
  = DatumHash Hash32
  | DatumInline BS.ByteString  -- tagged CBOR bytes (#6.24)
  deriving (Show, Eq)

decodeDatumOption :: Decoder s DatumOption
decodeDatumOption = do
  _ <- decodeListLen
  tag <- decodeWord
  case tag of
    0 -> DatumHash <$> decodeHash32
    1 -> do
      -- #6.24(bytes .cbor plutus_data) — just grab the tagged bytes
      _ <- decodeTag  -- tag 24
      DatumInline <$> decodeBytes
    _ -> fail $ "Unknown datum_option tag: " ++ show tag

data TransactionOutput = TransactionOutput
  { txOutAddress    :: BS.ByteString
  , txOutValue      :: Value
  , txOutDatumHash  :: Maybe Hash32       -- Alonzo style
  , txOutDatum      :: Maybe DatumOption  -- Babbage style
  , txOutScriptRef  :: Maybe BS.ByteString
  } deriving (Show, Eq)

decodeTransactionOutput :: Decoder s TransactionOutput
decodeTransactionOutput = do
  tt <- peekTokenType
  case tt of
    TypeListLen -> decodeAlonzoTxOut
    _           -> decodeBabbageTxOut

decodeAlonzoTxOut :: Decoder s TransactionOutput
decodeAlonzoTxOut = do
  len <- decodeListLen
  addr  <- decodeBytes
  value <- decodePositiveValue
  datumHash <- if len >= 3
    then Just <$> decodeHash32
    else return Nothing
  return $ TransactionOutput addr value datumHash Nothing Nothing

decodeBabbageTxOut :: Decoder s TransactionOutput
decodeBabbageTxOut = do
  len <- decodeMapLen
  go len (TransactionOutput BS.empty (ValueLovelace 0) Nothing Nothing Nothing)
  where
    go 0 acc = return acc
    go n acc = do
      key <- decodeWord
      case key of
        0 -> do
          addr <- decodeBytes
          go (n-1) acc { txOutAddress = addr }
        1 -> do
          val <- decodePositiveValue
          go (n-1) acc { txOutValue = val }
        2 -> do
          datum <- decodeDatumOption
          go (n-1) acc { txOutDatum = Just datum }
        3 -> do
          -- script_ref = #6.24(bytes .cbor script)
          _ <- decodeTag
          bs <- decodeBytes
          go (n-1) acc { txOutScriptRef = Just bs }
        _ -> do
          skipValue
          go (n-1) acc

--------------------------------------------------------------------------------
-- Certificates
--------------------------------------------------------------------------------

-- | Relay types for pool registration
data Relay
  = SingleHostAddr (Maybe Word.Word16) (Maybe BS.ByteString) (Maybe BS.ByteString)
  | SingleHostName (Maybe Word.Word16) T.Text
  | MultiHostName T.Text
  deriving (Show, Eq)

decodeRelay :: Decoder s Relay
decodeRelay = do
  _ <- decodeListLen
  tag <- decodeWord
  case tag of
    0 -> SingleHostAddr <$> decodeMaybe decodeWord16
                        <*> decodeMaybe decodeBytes
                        <*> decodeMaybe decodeBytes
    1 -> SingleHostName <$> decodeMaybe decodeWord16 <*> decodeString
    2 -> MultiHostName <$> decodeString
    _ -> fail $ "Unknown relay tag: " ++ show tag

data PoolMetadata = PoolMetadata
  { pmUrl  :: T.Text
  , pmHash :: BS.ByteString
  } deriving (Show, Eq)

decodePoolMetadata :: Decoder s PoolMetadata
decodePoolMetadata = do
  _ <- decodeListLen
  url  <- decodeString
  hash <- decodeBytes
  return $ PoolMetadata url hash

data PoolParams = PoolParams
  { ppOperator    :: Hash28
  , ppVrfKeyhash  :: Hash32
  , ppPledge      :: Word.Word64
  , ppCost        :: Word.Word64
  , ppMargin      :: (Word.Word64, Word.Word64) -- unit_interval as (num, denom)
  , ppRewardAccount :: BS.ByteString
  , ppPoolOwners  :: [Hash28]
  , ppRelays      :: [Relay]
  , ppPoolMetadata :: Maybe PoolMetadata
  } deriving (Show, Eq)

decodeUnitInterval :: Decoder s (Word.Word64, Word.Word64)
decodeUnitInterval = do
  _ <- decodeTag  -- tag 30
  _ <- decodeListLen
  num   <- decodeWord64
  denom <- decodeWord64
  return (num, denom)

decodePoolParams :: Decoder s PoolParams
decodePoolParams = do
  operator      <- decodeHash28
  vrfKeyhash    <- decodeHash32
  pledge        <- decodeWord64
  cost          <- decodeWord64
  margin        <- decodeUnitInterval
  rewardAccount <- decodeBytes
  poolOwners    <- decodeSet decodeHash28
  relays        <- decodeListOf decodeRelay
  poolMetadata  <- decodeMaybe decodePoolMetadata
  return $ PoolParams operator vrfKeyhash pledge cost margin
                      rewardAccount poolOwners relays poolMetadata

data Certificate
  = CertAccountRegistration Credential
  | CertAccountUnregistration Credential
  | CertDelegationToStakePool Credential Hash28
  | CertPoolRegistration PoolParams
  | CertPoolRetirement Hash28 Word.Word64
  | CertAccountRegistrationDeposit Credential Word.Word64
  | CertAccountUnregistrationDeposit Credential Word.Word64
  | CertDelegationToDRep Credential DRep
  | CertDelegationToStakePoolAndDRep Credential Hash28 DRep
  | CertAccountRegDelegStakePool Credential Hash28 Word.Word64
  | CertAccountRegDelegDRep Credential DRep Word.Word64
  | CertAccountRegDelegStakePoolAndDRep Credential Hash28 DRep Word.Word64
  | CertCommitteeAuth Credential Credential
  | CertCommitteeResignation Credential (Maybe Anchor)
  | CertDRepRegistration Credential Word.Word64 (Maybe Anchor)
  | CertDRepUnregistration Credential Word.Word64
  | CertDRepUpdate Credential (Maybe Anchor)
  deriving (Show, Eq)

decodeCertificate :: Decoder s Certificate
decodeCertificate = do
  _ <- decodeListLen
  tag <- decodeWord
  case tag of
    0  -> CertAccountRegistration <$> decodeCredential
    1  -> CertAccountUnregistration <$> decodeCredential
    2  -> CertDelegationToStakePool <$> decodeCredential <*> decodeHash28
    3  -> CertPoolRegistration <$> decodePoolParams
    4  -> CertPoolRetirement <$> decodeHash28 <*> decodeWord64
    7  -> CertAccountRegistrationDeposit <$> decodeCredential <*> decodeWord64
    8  -> CertAccountUnregistrationDeposit <$> decodeCredential <*> decodeWord64
    9  -> CertDelegationToDRep <$> decodeCredential <*> decodeDRep
    10 -> CertDelegationToStakePoolAndDRep <$> decodeCredential <*> decodeHash28 <*> decodeDRep
    11 -> CertAccountRegDelegStakePool <$> decodeCredential <*> decodeHash28 <*> decodeWord64
    12 -> CertAccountRegDelegDRep <$> decodeCredential <*> decodeDRep <*> decodeWord64
    13 -> CertAccountRegDelegStakePoolAndDRep <$> decodeCredential <*> decodeHash28 <*> decodeDRep <*> decodeWord64
    14 -> CertCommitteeAuth <$> decodeCredential <*> decodeCredential
    15 -> CertCommitteeResignation <$> decodeCredential <*> decodeMaybe decodeAnchor
    16 -> CertDRepRegistration <$> decodeCredential <*> decodeWord64 <*> decodeMaybe decodeAnchor
    17 -> CertDRepUnregistration <$> decodeCredential <*> decodeWord64
    18 -> CertDRepUpdate <$> decodeCredential <*> decodeMaybe decodeAnchor
    _  -> fail $ "Unknown certificate tag: " ++ show tag

--------------------------------------------------------------------------------
-- Native Script
--------------------------------------------------------------------------------

data NativeScript
  = ScriptPubkey Hash28
  | ScriptAll [NativeScript]
  | ScriptAny [NativeScript]
  | ScriptNOfK Int64 [NativeScript]
  | ScriptInvalidBefore Word.Word64
  | ScriptInvalidHereafter Word.Word64
  deriving (Show, Eq)

decodeNativeScript :: Decoder s NativeScript
decodeNativeScript = do
  _ <- decodeListLen
  tag <- decodeWord
  case tag of
    0 -> ScriptPubkey <$> decodeHash28
    1 -> ScriptAll <$> decodeListOf decodeNativeScript
    2 -> ScriptAny <$> decodeListOf decodeNativeScript
    3 -> ScriptNOfK <$> decodeInt64 <*> decodeListOf decodeNativeScript
    4 -> ScriptInvalidBefore <$> decodeWord64
    5 -> ScriptInvalidHereafter <$> decodeWord64
    _ -> fail $ "Unknown native script tag: " ++ show tag

--------------------------------------------------------------------------------
-- Plutus Data
--------------------------------------------------------------------------------

data PlutusData
  = PlutusConstr Word PlutusConstrFields
  | PlutusMap [(PlutusData, PlutusData)]
  | PlutusList [PlutusData]
  | PlutusInteger Integer
  | PlutusBytes BS.ByteString
  | PlutusBigUInt BS.ByteString
  | PlutusBigNInt BS.ByteString
  deriving (Show, Eq)

-- | Constructor fields: tag and list of data
data PlutusConstrFields = PlutusConstrFields
  { pcfTag    :: Word  -- the alternative index
  , pcfFields :: [PlutusData]
  } deriving (Show, Eq)

decodePlutusData :: Decoder s PlutusData
decodePlutusData = do
  tt <- peekTokenType
  case tt of
    TypeTag -> do
      tag <- decodeTag
      if tag >= 121 && tag <= 127
        then do
          fields <- decodeListOf decodePlutusData
          return $ PlutusConstr tag (PlutusConstrFields (tag - 121) fields)
        else if tag == 102
          then do
            _ <- decodeListLen
            alt <- decodeWord
            fields <- decodeListOf decodePlutusData
            return $ PlutusConstr tag (PlutusConstrFields alt fields)
          else if tag == 2
            then PlutusBigUInt <$> decodeBytes
            else if tag == 3
              then PlutusBigNInt <$> decodeBytes
              else fail $ "Unknown plutus data tag: " ++ show tag
    TypeMapLen -> do
      len <- decodeMapLen
      pairs <- replicateM len $ do
        k <- decodePlutusData
        v <- decodePlutusData
        return (k, v)
      return $ PlutusMap pairs
    TypeMapLenIndef -> do
      pairs <- decodeIndefMapList
      return $ PlutusMap pairs
    TypeListLen -> PlutusList <$> decodeListOf decodePlutusData
    TypeListLenIndef -> PlutusList <$> decodeListOf decodePlutusData
    TypeUInt   -> PlutusInteger <$> decodeInteger
    TypeUInt64 -> PlutusInteger <$> decodeInteger
    TypeNInt   -> PlutusInteger <$> decodeInteger
    TypeNInt64 -> PlutusInteger <$> decodeInteger
    TypeInteger -> PlutusInteger <$> decodeInteger
    TypeBytes  -> PlutusBytes <$> decodeBytes
    TypeBytesIndef -> PlutusBytes <$> decodeBytes
    _          -> fail $ "Unexpected token type in plutus_data: " ++ show tt
  where
    decodeIndefMapList = do
      void decodeMapLenOrIndef -- consume the indef marker
      go []
      where
        go acc = do
          done <- decodeBreakOr
          if done
            then return (reverse acc)
            else do
              k <- decodePlutusData
              v <- decodePlutusData
              go ((k, v) : acc)


--------------------------------------------------------------------------------
-- VKey Witness
--------------------------------------------------------------------------------

data VKeyWitness = VKeyWitness
  { vkwVKey      :: VKey
  , vkwSignature :: BS.ByteString
  } deriving (Show, Eq)

decodeVKeyWitness :: Decoder s VKeyWitness
decodeVKeyWitness = do
  _ <- decodeListLen
  vkey <- decodeVKey
  sig  <- decodeBytes
  return $ VKeyWitness vkey sig

--------------------------------------------------------------------------------
-- Bootstrap Witness
--------------------------------------------------------------------------------

data BootstrapWitness = BootstrapWitness
  { bwPublicKey  :: VKey
  , bwSignature  :: BS.ByteString
  , bwChainCode  :: BS.ByteString
  , bwAttributes :: BS.ByteString
  } deriving (Show, Eq)

decodeBootstrapWitness :: Decoder s BootstrapWitness
decodeBootstrapWitness = do
  _ <- decodeListLen
  pk   <- decodeVKey
  sig  <- decodeBytes
  cc   <- decodeBytes
  attr <- decodeBytes
  return $ BootstrapWitness pk sig cc attr

--------------------------------------------------------------------------------
-- Redeemer
--------------------------------------------------------------------------------

data ExUnits = ExUnits
  { exMem   :: Word.Word64
  , exSteps :: Word.Word64
  } deriving (Show, Eq)

decodeExUnits :: Decoder s ExUnits
decodeExUnits = do
  _ <- decodeListLen
  mem   <- decodeWord64
  steps <- decodeWord64
  return $ ExUnits mem steps

data Redeemer = Redeemer
  { rdTag     :: Word.Word8
  , rdIndex   :: Word.Word32
  , rdData    :: PlutusData
  , rdExUnits :: ExUnits
  } deriving (Show, Eq)

decodeRedeemer :: Decoder s Redeemer
decodeRedeemer = do
  _ <- decodeListLen
  tag     <- decodeWord8
  index   <- decodeWord32
  pdata   <- decodePlutusData
  exunits <- decodeExUnits
  return $ Redeemer tag index pdata exunits

-- | Redeemers: array format [+ redeemer] or map format {[tag, index] => [data, ex_units]}
decodeRedeemers :: Decoder s [Redeemer]
decodeRedeemers = do
  tt <- peekTokenType
  case tt of
    TypeListLen     -> decodeListOf decodeRedeemer
    TypeListLenIndef -> decodeListOf decodeRedeemer
    TypeMapLen      -> decodeRedeemersMap
    TypeMapLenIndef -> decodeRedeemersMap
    _               -> fail $ "Unexpected token type for redeemers: " ++ show tt

decodeRedeemersMap :: Decoder s [Redeemer]
decodeRedeemersMap = do
  len <- decodeMapLen
  replicateM len $ do
    _ <- decodeListLen
    tag   <- decodeWord8
    index <- decodeWord32
    _ <- decodeListLen
    pdata   <- decodePlutusData
    exunits <- decodeExUnits
    return $ Redeemer tag index pdata exunits

--------------------------------------------------------------------------------
-- Transaction Witness Set
--------------------------------------------------------------------------------

data TransactionWitnessSet = TransactionWitnessSet
  { twsVKeyWitnesses     :: [VKeyWitness]
  , twsNativeScripts     :: [NativeScript]
  , twsBootstrapWitnesses :: [BootstrapWitness]
  , twsPlutusV1Scripts   :: [BS.ByteString]
  , twsPlutusData        :: [PlutusData]
  , twsRedeemers         :: [Redeemer]
  , twsPlutusV2Scripts   :: [BS.ByteString]
  , twsPlutusV3Scripts   :: [BS.ByteString]
  } deriving (Show, Eq)

emptyWitnessSet :: TransactionWitnessSet
emptyWitnessSet = TransactionWitnessSet [] [] [] [] [] [] [] []

decodeTransactionWitnessSet :: Decoder s TransactionWitnessSet
decodeTransactionWitnessSet = do
  len <- decodeMapLen
  go len emptyWitnessSet
  where
    go 0 acc = return acc
    go n acc = do
      key <- decodeWord
      case key of
        0 -> do ws <- decodeNonemptySet decodeVKeyWitness
                go (n-1) acc { twsVKeyWitnesses = ws }
        1 -> do ss <- decodeNonemptySet decodeNativeScript
                go (n-1) acc { twsNativeScripts = ss }
        2 -> do bs <- decodeNonemptySet decodeBootstrapWitness
                go (n-1) acc { twsBootstrapWitnesses = bs }
        3 -> do ps <- decodeNonemptySet decodeBytes
                go (n-1) acc { twsPlutusV1Scripts = ps }
        4 -> do ds <- decodeNonemptySet decodePlutusData
                go (n-1) acc { twsPlutusData = ds }
        5 -> do rs <- decodeRedeemers
                go (n-1) acc { twsRedeemers = rs }
        6 -> do ps <- decodeNonemptySet decodeBytes
                go (n-1) acc { twsPlutusV2Scripts = ps }
        7 -> do ps <- decodeNonemptySet decodeBytes
                go (n-1) acc { twsPlutusV3Scripts = ps }
        _ -> do skipValue
                go (n-1) acc

--------------------------------------------------------------------------------
-- Governance types
--------------------------------------------------------------------------------

data Voter
  = VoterCommitteeKeyHash Hash28
  | VoterCommitteeScriptHash Hash28
  | VoterDRepKeyHash Hash28
  | VoterDRepScriptHash Hash28
  | VoterStakePoolKeyHash Hash28
  deriving (Show, Eq, Ord)

decodeVoter :: Decoder s Voter
decodeVoter = do
  _ <- decodeListLen
  tag <- decodeWord
  case tag of
    0 -> VoterCommitteeKeyHash <$> decodeHash28
    1 -> VoterCommitteeScriptHash <$> decodeHash28
    2 -> VoterDRepKeyHash <$> decodeHash28
    3 -> VoterDRepScriptHash <$> decodeHash28
    4 -> VoterStakePoolKeyHash <$> decodeHash28
    _ -> fail $ "Unknown voter tag: " ++ show tag

data GovActionId = GovActionId
  { gaiTxId  :: Hash32
  , gaiIndex :: Word.Word16
  } deriving (Show, Eq, Ord)

decodeGovActionId :: Decoder s GovActionId
decodeGovActionId = do
  _ <- decodeListLen
  txId  <- decodeHash32
  index <- decodeWord16
  return $ GovActionId txId index

data VotingProcedure = VotingProcedure
  { vpVote   :: Word.Word8
  , vpAnchor :: Maybe Anchor
  } deriving (Show, Eq)

decodeVotingProcedure :: Decoder s VotingProcedure
decodeVotingProcedure = do
  _ <- decodeListLen
  vote   <- decodeWord8
  anchor <- decodeMaybe decodeAnchor
  return $ VotingProcedure vote anchor

type VotingProcedures = Map.Map Voter (Map.Map GovActionId VotingProcedure)

decodeVotingProcedures :: Decoder s VotingProcedures
decodeVotingProcedures = decodeMapOf decodeVoter (decodeMapOf decodeGovActionId decodeVotingProcedure)

-- | Gov actions stored as opaque CBOR bytes for pragmatism
--   (the full protocol_param_update type is very complex)
data GovAction
  = GovActionParameterChange (Maybe GovActionId) BS.ByteString (Maybe Hash28) -- protocol_param_update as raw bytes
  | GovActionHardFork (Maybe GovActionId) ProtocolVersion
  | GovActionTreasuryWithdrawals (Map.Map BS.ByteString Word.Word64) (Maybe Hash28)
  | GovActionNoConfidence (Maybe GovActionId)
  | GovActionUpdateCommittee (Maybe GovActionId) [Credential] (Map.Map Credential Word.Word64) (Word.Word64, Word.Word64)
  | GovActionNewConstitution (Maybe GovActionId) Anchor (Maybe Hash28)
  | GovActionInfo
  deriving (Show, Eq)

decodeGovAction :: Decoder s GovAction
decodeGovAction = do
  _ <- decodeListLen
  tag <- decodeWord
  case tag of
    0 -> do
      prevId <- decodeMaybe decodeGovActionId
      -- protocol_param_update is a complex map — decode as raw bytes not feasible
      -- Instead, skip the value entirely
      ppUpdate <- decodeProtocolParamUpdateRaw
      guardrail <- decodeMaybe decodeHash28
      return $ GovActionParameterChange prevId ppUpdate guardrail
    1 -> GovActionHardFork <$> decodeMaybe decodeGovActionId <*> decodeProtocolVersion
    2 -> do
      withdrawals <- decodeMapOf decodeBytes decodeWord64
      guardrail <- decodeMaybe decodeHash28
      return $ GovActionTreasuryWithdrawals withdrawals guardrail
    3 -> GovActionNoConfidence <$> decodeMaybe decodeGovActionId
    4 -> do
      prevId <- decodeMaybe decodeGovActionId
      removals <- decodeSet decodeCredential
      additions <- decodeMapOf decodeCredential decodeWord64
      threshold <- decodeUnitInterval
      return $ GovActionUpdateCommittee prevId removals additions threshold
    5 -> do
      prevId <- decodeMaybe decodeGovActionId
      anchor <- decodeAnchor
      guardrail <- decodeMaybe decodeHash28
      return $ GovActionNewConstitution prevId anchor guardrail
    6 -> return GovActionInfo
    _ -> fail $ "Unknown gov_action tag: " ++ show tag

-- | For protocol_param_update we skip it as raw — just consume the CBOR map
decodeProtocolParamUpdateRaw :: Decoder s BS.ByteString
decodeProtocolParamUpdateRaw = do
  len <- decodeMapLen
  replicateM_ len $ do
    _ <- decodeWord  -- key
    skipValue        -- value
  return BS.empty

data ProposalProcedure = ProposalProcedure
  { propDeposit       :: Word.Word64
  , propRewardAccount :: BS.ByteString
  , propGovAction     :: GovAction
  , propAnchor        :: Anchor
  } deriving (Show, Eq)

decodeProposalProcedure :: Decoder s ProposalProcedure
decodeProposalProcedure = do
  _ <- decodeListLen
  deposit       <- decodeWord64
  rewardAccount <- decodeBytes
  govAction     <- decodeGovAction
  anchor        <- decodeAnchor
  return $ ProposalProcedure deposit rewardAccount govAction anchor

--------------------------------------------------------------------------------
-- Metadatum / Auxiliary Data
--------------------------------------------------------------------------------

data Metadatum
  = MetadatumMap [(Metadatum, Metadatum)]
  | MetadatumList [Metadatum]
  | MetadatumInt Integer
  | MetadatumBytes BS.ByteString
  | MetadatumText T.Text
  deriving (Show, Eq)

decodeMetadatum :: Decoder s Metadatum
decodeMetadatum = do
  tt <- peekTokenType
  case tt of
    TypeMapLen -> do
      len <- decodeMapLen
      pairs <- replicateM len $ do
        k <- decodeMetadatum
        v <- decodeMetadatum
        return (k, v)
      return $ MetadatumMap pairs
    TypeMapLenIndef -> do
      void decodeMapLenOrIndef
      pairs <- decodeIndefMetadatumMap
      return $ MetadatumMap pairs
    TypeListLen -> do
      MetadatumList <$> decodeListOf decodeMetadatum
    TypeListLenIndef ->
      MetadatumList <$> decodeListOf decodeMetadatum
    TypeUInt    -> MetadatumInt <$> decodeInteger
    TypeUInt64  -> MetadatumInt <$> decodeInteger
    TypeNInt    -> MetadatumInt <$> decodeInteger
    TypeNInt64  -> MetadatumInt <$> decodeInteger
    TypeInteger -> MetadatumInt <$> decodeInteger
    TypeBytes      -> MetadatumBytes <$> decodeBytes
    TypeBytesIndef -> MetadatumBytes <$> decodeBytes
    TypeString      -> MetadatumText <$> decodeString
    TypeStringIndef -> MetadatumText <$> decodeString
    _ -> fail $ "Unexpected token type in metadatum: " ++ show tt
  where
    decodeIndefMetadatumMap = go []
      where
        go acc = do
          done <- decodeBreakOr
          if done
            then return (reverse acc)
            else do
              k <- decodeMetadatum
              v <- decodeMetadatum
              go ((k, v) : acc)

type Metadata = Map.Map Word.Word64 Metadatum

decodeMetadata :: Decoder s Metadata
decodeMetadata = decodeMapOf decodeWord64 decodeMetadatum

data AuxiliaryData
  = AuxMetadata Metadata
  | AuxArray Metadata [NativeScript]
  | AuxMap
      { auxMetadata      :: Maybe Metadata
      , auxNativeScripts :: [NativeScript]
      , auxPlutusV1      :: [BS.ByteString]
      , auxPlutusV2      :: [BS.ByteString]
      , auxPlutusV3      :: [BS.ByteString]
      }
  deriving (Show, Eq)

decodeAuxiliaryData :: Decoder s AuxiliaryData
decodeAuxiliaryData = do
  tt <- peekTokenType
  case tt of
    TypeTag -> do
      _ <- decodeTag  -- tag 259
      decodeAuxMap
    TypeMapLen -> AuxMetadata <$> decodeMetadata
    TypeMapLenIndef -> AuxMetadata <$> decodeMetadata
    TypeListLen -> do
      _ <- decodeListLen
      meta    <- decodeMetadata
      scripts <- decodeListOf decodeNativeScript
      return $ AuxArray meta scripts
    _ -> fail $ "Unexpected token type for auxiliary_data: " ++ show tt

decodeAuxMap :: Decoder s AuxiliaryData
decodeAuxMap = do
  len <- decodeMapLen
  go len (AuxMap Nothing [] [] [] [])
  where
    go 0 acc = return acc
    go n acc = do
      key <- decodeWord
      case key of
        0 -> do meta <- decodeMetadata
                go (n-1) acc { auxMetadata = Just meta }
        1 -> do ss <- decodeListOf decodeNativeScript
                go (n-1) acc { auxNativeScripts = ss }
        2 -> do ps <- decodeListOf decodeBytes
                go (n-1) acc { auxPlutusV1 = ps }
        3 -> do ps <- decodeListOf decodeBytes
                go (n-1) acc { auxPlutusV2 = ps }
        4 -> do ps <- decodeListOf decodeBytes
                go (n-1) acc { auxPlutusV3 = ps }
        _ -> do skipValue
                go (n-1) acc

--------------------------------------------------------------------------------
-- Transaction Body
--------------------------------------------------------------------------------

data TransactionBody = TransactionBody
  { tbInputs               :: [TransactionInput]
  , tbOutputs              :: [TransactionOutput]
  , tbFee                  :: Word.Word64
  , tbTtl                  :: Maybe Word.Word64
  , tbCertificates         :: [Certificate]
  , tbWithdrawals          :: Map.Map BS.ByteString Word.Word64
  , tbAuxiliaryDataHash    :: Maybe Hash32
  , tbValidityStart        :: Maybe Word.Word64
  , tbMint                 :: Map.Map PolicyId (Map.Map AssetName Integer)
  , tbScriptDataHash       :: Maybe Hash32
  , tbCollateral           :: [TransactionInput]
  , tbRequiredSigners      :: [Hash28]
  , tbNetworkId            :: Maybe Word.Word8
  , tbCollateralReturn     :: Maybe TransactionOutput
  , tbTotalCollateral      :: Maybe Word.Word64
  , tbReferenceInputs      :: [TransactionInput]
  , tbVotingProcedures     :: Maybe VotingProcedures
  , tbProposalProcedures   :: [ProposalProcedure]
  , tbTreasuryValue        :: Maybe Word.Word64
  , tbDonation             :: Maybe Word.Word64
  } deriving (Show, Eq)

emptyTransactionBody :: TransactionBody
emptyTransactionBody = TransactionBody
  { tbInputs             = []
  , tbOutputs            = []
  , tbFee                = 0
  , tbTtl                = Nothing
  , tbCertificates       = []
  , tbWithdrawals        = Map.empty
  , tbAuxiliaryDataHash  = Nothing
  , tbValidityStart      = Nothing
  , tbMint               = Map.empty
  , tbScriptDataHash     = Nothing
  , tbCollateral         = []
  , tbRequiredSigners    = []
  , tbNetworkId          = Nothing
  , tbCollateralReturn   = Nothing
  , tbTotalCollateral    = Nothing
  , tbReferenceInputs    = []
  , tbVotingProcedures   = Nothing
  , tbProposalProcedures = []
  , tbTreasuryValue      = Nothing
  , tbDonation           = Nothing
  }

decodeTransactionBody :: Decoder s TransactionBody
decodeTransactionBody = do
  len <- decodeMapLen
  go len emptyTransactionBody
  where
    go 0 acc = return acc
    go n acc = do
      key <- decodeWord
      case key of
        0  -> do inputs <- decodeSet decodeTransactionInput
                 go (n-1) acc { tbInputs = inputs }
        1  -> do outputs <- decodeListOf decodeTransactionOutput
                 go (n-1) acc { tbOutputs = outputs }
        2  -> do fee <- decodeWord64
                 go (n-1) acc { tbFee = fee }
        3  -> do ttl <- decodeWord64
                 go (n-1) acc { tbTtl = Just ttl }
        4  -> do certs <- decodeNonemptySet decodeCertificate
                 go (n-1) acc { tbCertificates = certs }
        5  -> do w <- decodeMapOf decodeBytes decodeWord64
                 go (n-1) acc { tbWithdrawals = w }
        7  -> do h <- decodeHash32
                 go (n-1) acc { tbAuxiliaryDataHash = Just h }
        8  -> do s <- decodeWord64
                 go (n-1) acc { tbValidityStart = Just s }
        9  -> do m <- decodeMultiAsset decodeInteger
                 go (n-1) acc { tbMint = m }
        11 -> do h <- decodeHash32
                 go (n-1) acc { tbScriptDataHash = Just h }
        13 -> do c <- decodeNonemptySet decodeTransactionInput
                 go (n-1) acc { tbCollateral = c }
        14 -> do s <- decodeNonemptySet decodeHash28
                 go (n-1) acc { tbRequiredSigners = s }
        15 -> do nid <- decodeWord8
                 go (n-1) acc { tbNetworkId = Just nid }
        16 -> do o <- decodeTransactionOutput
                 go (n-1) acc { tbCollateralReturn = Just o }
        17 -> do c <- decodeWord64
                 go (n-1) acc { tbTotalCollateral = Just c }
        18 -> do r <- decodeNonemptySet decodeTransactionInput
                 go (n-1) acc { tbReferenceInputs = r }
        19 -> do v <- decodeVotingProcedures
                 go (n-1) acc { tbVotingProcedures = Just v }
        20 -> do p <- decodeNonemptySet decodeProposalProcedure
                 go (n-1) acc { tbProposalProcedures = p }
        21 -> do t <- decodeWord64
                 go (n-1) acc { tbTreasuryValue = Just t }
        22 -> do d <- decodeWord64
                 go (n-1) acc { tbDonation = Just d }
        _  -> do skipValue
                 go (n-1) acc

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

data Block = Block
  { bHeader               :: Header
  , bTransactionBodies    :: [TransactionBody]
  , bTransactionWitnessSets :: [TransactionWitnessSet]
  , bAuxiliaryDataSet     :: Map.Map TransactionIndex AuxiliaryData
  , bInvalidTransactions  :: [TransactionIndex]
  } deriving (Show, Eq)

-- | Decode a block, handling the era wrapper [era_id, block_array] if present
decodeBlock :: Decoder s Block
decodeBlock = do
  len <- decodeListLen
  case len of
    2 -> do
      -- Era wrapper: [era_id, block_array]
      tt <- peekTokenType
      case tt of
        TypeUInt   -> do _ <- decodeWord  -- consume era id
                         decodeBlockBody
        TypeUInt64 -> do _ <- decodeWord64
                         decodeBlockBody
        _          -> decodeBlockWith2 -- it's just a 2-element structure
    5 -> decodeBlockFields
    _ -> fail $ "Unexpected block array length: " ++ show len

-- | When we already consumed a list of length 2 and the first element is not a uint
decodeBlockWith2 :: Decoder s Block
decodeBlockWith2 = fail "Expected era wrapper [uint, block] but first element is not uint"

decodeBlockBody :: Decoder s Block
decodeBlockBody = do
  _ <- decodeListLen  -- should be 5
  decodeBlockFields

decodeBlockFields :: Decoder s Block
decodeBlockFields = do
  header              <- decodeHeader
  transactionBodies   <- decodeListOf decodeTransactionBody
  transactionWitnessSets <- decodeListOf decodeTransactionWitnessSet
  auxiliaryDataSet    <- decodeMapOf decodeWord16 decodeAuxiliaryData
  invalidTransactions <- decodeListOf decodeWord16
  return $ Block header transactionBodies transactionWitnessSets
                 auxiliaryDataSet invalidTransactions

--------------------------------------------------------------------------------
-- Skip arbitrary CBOR value
--------------------------------------------------------------------------------

skipValue :: Decoder s ()
skipValue = do
  tt <- peekTokenType
  case tt of
    TypeUInt      -> void decodeWord64
    TypeUInt64    -> void decodeWord64
    TypeNInt      -> void decodeInteger
    TypeNInt64    -> void decodeInteger
    TypeInteger   -> void decodeInteger
    TypeBytes     -> void decodeBytes
    TypeBytesIndef -> void decodeBytes
    TypeString    -> void decodeString
    TypeStringIndef -> void decodeString
    TypeBool      -> void decodeBool
    TypeNull      -> decodeNull
    TypeTag       -> do
      _ <- decodeTag
      skipValue
    TypeListLen   -> do
      len <- decodeListLen
      replicateM_ len skipValue
    TypeListLenIndef -> do
      void decodeListLenOrIndef
      skipIndefList
    TypeMapLen    -> do
      len <- decodeMapLen
      replicateM_ len $ do
        skipValue
        skipValue
    TypeMapLenIndef -> do
      void decodeMapLenOrIndef
      skipIndefMap
    TypeFloat16   -> void decodeWord
    TypeFloat32   -> void decodeWord
    TypeFloat64   -> void decodeWord64
    TypeSimple    -> void decodeWord8
    TypeBreak     -> void decodeBreakOr
    _             -> fail $ "skipValue: unexpected token type " ++ show tt

skipIndefList :: Decoder s ()
skipIndefList = do
  done <- decodeBreakOr
  if done then return ()
  else do
    skipValue
    skipIndefList

skipIndefMap :: Decoder s ()
skipIndefMap = do
  done <- decodeBreakOr
  if done then return ()
  else do
    skipValue
    skipValue
    skipIndefMap

