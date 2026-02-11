{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Conway.Pretty where

import Cardano.Conway.Block
import qualified Codec.Binary.Bech32 as Bech32
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List (intercalate)

-- | Pretty print a block.
prettyBlock :: Block -> String
prettyBlock Block{..} =
  unlines $
    [ "Cardano Era 7 Block"
    , ""
    , "Header Body"
    , prettyHeaderBody (hHeaderBody bHeader)
    ]
    ++ map (\(i, tb) -> prettyTransaction i tb) (zip [0..] bTransactionBodies)
    ++ map (\(i, ws) -> prettyWitnessSet i ws) (zip [0..] bTransactionWitnessSets)
    ++ map (\entry -> prettyAuxData entry) (Map.toList bAuxiliaryDataSet)

prettyHeaderBody :: HeaderBody -> String
prettyHeaderBody HeaderBody{..} =
  unlines
    [ "  Block Number:   " ++ show hbBlockNumber
    , "  Slot:           " ++ show hbSlot
    , "  Prev Hash:      " ++ maybe "None" prettyHash32 hbPrevHash
    , "  Issuer VKey:    " ++ prettyVKey hbIssuerVKey
    , "  VRF VKey:       " ++ prettyVrfVKey hbVrfVKey
    , "  VRF Certificate:"
    , "    Output: " ++ prettyVrfCertOutput hbVrfResult
    , "    Proof:  " ++ prettyVrfCertProof hbVrfResult
    , "  Block Body Hash: " ++ prettyHash32 hbBlockBodyHash
    , "  Body Size:      " ++ show hbBlockBodySize ++ " bytes"
    , "  Operational Cert:"
    , prettyOperationalCert hbOperationalCert
    , "  Protocol Version: " ++ show (pvMajor hbProtocolVersion) ++ "." ++ show (pvMinor hbProtocolVersion)
    ]

prettyHash32 :: Hash32 -> String
prettyHash32 (Hash32 bs) = BS.Char8.unpack $ Base16.encode bs

prettyHash28 :: Hash28 -> String
prettyHash28 (Hash28 bs) = BS.Char8.unpack $ Base16.encode bs

prettyVKey :: VKey -> String
prettyVKey (VKey bs) = BS.Char8.unpack $ Base16.encode bs

prettyVrfVKey :: VrfVKey -> String
prettyVrfVKey (VrfVKey bs) = BS.Char8.unpack $ Base16.encode bs

prettyVrfCertOutput :: VrfCert -> String
prettyVrfCertOutput (VrfCert output _) = BS.Char8.unpack $ Base16.encode output

prettyVrfCertProof :: VrfCert -> String
prettyVrfCertProof (VrfCert _ proof) = BS.Char8.unpack $ Base16.encode proof

prettyOperationalCert :: OperationalCert -> String
prettyOperationalCert OperationalCert{..} =
  intercalate "\n"
    [ "    Hot VKey:    " ++ prettyVKey opCertHotVKey
    , "    Seq Number:  " ++ show opCertSeqNum
    , "    KES Period:  " ++ show opCertKesPeriod
    , "    Sigma:       " ++ prettyBS opCertSigma
    ]

prettyBS :: BS.ByteString -> String
prettyBS = BS.Char8.unpack . Base16.encode

-- | Encode an address in bech32, falling back to hex for Byron or unknown types.
prettyAddress :: BS.ByteString -> String
prettyAddress addr
  | BS.null addr = "<empty>"
  | otherwise =
      let headerByte = BS.index addr 0
          addrType = shiftR headerByte 4
          networkId = headerByte .&. 0x0F
          mHrpText
            | addrType <= 7 = Just $ if networkId == 1 then "addr" else "addr_test"
            | addrType >= 14 = Just $ if networkId == 1 then "stake" else "stake_test"
            | otherwise = Nothing  -- Byron (8) or unknown
      in case mHrpText of
           Nothing -> prettyBS addr
           Just hrpText ->
             case Bech32.humanReadablePartFromText hrpText of
               Left _ -> prettyBS addr
               Right hrp ->
                 let dp = Bech32.dataPartFromBytes addr
                 in T.unpack (Bech32.encodeLenient hrp dp)

--------------------------------------------------------------------------------
-- Transaction pretty printing
--------------------------------------------------------------------------------

prettyTransaction :: Int -> TransactionBody -> String
prettyTransaction idx tb =
  unlines $
    [ "Transaction " ++ show idx
    , "  Inputs: " ++ show (length (tbInputs tb))
    ]
    ++ map (\inp -> "    " ++ prettyTxInput inp) (tbInputs tb)
    ++
    [ "  Outputs: " ++ show (length (tbOutputs tb))
    ]
    ++ concatMap (\(i, out) -> prettyTxOutput i out) (zip [0..] (tbOutputs tb))
    ++
    [ "  Fee: " ++ show (tbFee tb) ++ " lovelace"
    ]
    ++ maybe [] (\ttl -> ["  TTL: " ++ show ttl]) (tbTtl tb)
    ++ (if null (tbCertificates tb) then []
        else ("  Certificates: " ++ show (length (tbCertificates tb)))
             : map (\c -> "    " ++ prettyCertificate c) (tbCertificates tb))
    ++ (if Map.null (tbWithdrawals tb) then []
        else ["  Withdrawals: " ++ show (Map.size (tbWithdrawals tb))]
             ++ map (\(addr, coin) -> "    " ++ prettyAddress addr ++ ": " ++ show coin ++ " lovelace")
                    (Map.toList (tbWithdrawals tb)))
    ++ maybe [] (\h -> ["  Auxiliary Data Hash: " ++ prettyHash32 h]) (tbAuxiliaryDataHash tb)
    ++ maybe [] (\s -> ["  Validity Start: " ++ show s]) (tbValidityStart tb)
    ++ (if Map.null (tbMint tb) then []
        else ["  Mint:"] ++ prettyMultiAsset (tbMint tb))
    ++ maybe [] (\h -> ["  Script Data Hash: " ++ prettyHash32 h]) (tbScriptDataHash tb)
    ++ (if null (tbCollateral tb) then []
        else ("  Collateral: " ++ show (length (tbCollateral tb)))
             : map (\inp -> "    " ++ prettyTxInput inp) (tbCollateral tb))
    ++ (if null (tbRequiredSigners tb) then []
        else ("  Required Signers: " ++ show (length (tbRequiredSigners tb)))
             : map (\h -> "    " ++ prettyHash28 h) (tbRequiredSigners tb))
    ++ maybe [] (\nid -> ["  Network ID: " ++ show nid]) (tbNetworkId tb)
    ++ maybe [] (\out -> "  Collateral Return:" : prettyTxOutput 0 out) (tbCollateralReturn tb)
    ++ maybe [] (\c -> ["  Total Collateral: " ++ show c ++ " lovelace"]) (tbTotalCollateral tb)
    ++ (if null (tbReferenceInputs tb) then []
        else ("  Reference Inputs: " ++ show (length (tbReferenceInputs tb)))
             : map (\inp -> "    " ++ prettyTxInput inp) (tbReferenceInputs tb))
    ++ maybe [] (\_ -> ["  Voting Procedures: Present"]) (tbVotingProcedures tb)
    ++ (if null (tbProposalProcedures tb) then []
        else ["  Proposal Procedures: " ++ show (length (tbProposalProcedures tb))])
    ++ maybe [] (\t -> ["  Treasury Value: " ++ show t ++ " lovelace"]) (tbTreasuryValue tb)
    ++ maybe [] (\d -> ["  Donation: " ++ show d ++ " lovelace"]) (tbDonation tb)

prettyTxInput :: TransactionInput -> String
prettyTxInput TransactionInput{..} =
  prettyHash32 txInputId ++ "#" ++ show txInputIndex

prettyTxOutput :: Int -> TransactionOutput -> [String]
prettyTxOutput idx TransactionOutput{..} =
  [ "    Output " ++ show idx ++ ":"
  , "      Address: " ++ prettyAddress txOutAddress
  , "      Value:   " ++ prettyValue txOutValue
  ]
  ++ maybe [] (\h -> ["      Datum Hash: " ++ prettyHash32 h]) txOutDatumHash
  ++ maybe [] (\d -> ["      Datum: " ++ prettyDatumOption d]) txOutDatum
  ++ maybe [] (\s -> ["      Script Ref: " ++ prettyBS s]) txOutScriptRef

prettyValue :: Value -> String
prettyValue (ValueLovelace coin) = show coin ++ " lovelace"
prettyValue (ValueMultiAsset coin assets) =
  show coin ++ " lovelace"
  ++ if Map.null assets then ""
     else " + " ++ intercalate ", " (concatMap prettyPolicy (Map.toList assets))
  where
    prettyPolicy (pid, assetMap) =
      map (\(name, amt) ->
        show amt ++ " " ++ prettyHash28 pid ++ "." ++ prettyBS name)
        (Map.toList assetMap)

prettyDatumOption :: DatumOption -> String
prettyDatumOption (DatumHash h) = "Hash " ++ prettyHash32 h
prettyDatumOption (DatumInline bs) = "Inline (" ++ show (BS.length bs) ++ " bytes)"

prettyMultiAsset :: Map.Map PolicyId (Map.Map AssetName Integer) -> [String]
prettyMultiAsset assets =
  concatMap (\(pid, assetMap) ->
    map (\(name, amt) ->
      "    " ++ show amt ++ " " ++ prettyHash28 pid ++ "." ++ prettyBS name)
      (Map.toList assetMap))
    (Map.toList assets)

--------------------------------------------------------------------------------
-- Certificate pretty printing
--------------------------------------------------------------------------------

prettyCertificate :: Certificate -> String
prettyCertificate cert = case cert of
  CertAccountRegistration cred ->
    "Account Registration: " ++ prettyCredential cred
  CertAccountUnregistration cred ->
    "Account Unregistration: " ++ prettyCredential cred
  CertDelegationToStakePool cred pool ->
    "Delegation to Pool: " ++ prettyCredential cred ++ " -> " ++ prettyHash28 pool
  CertPoolRegistration pp ->
    "Pool Registration: operator=" ++ prettyHash28 (ppOperator pp)
  CertPoolRetirement pool epoch ->
    "Pool Retirement: " ++ prettyHash28 pool ++ " at epoch " ++ show epoch
  CertAccountRegistrationDeposit cred deposit ->
    "Account Registration (deposit " ++ show deposit ++ "): " ++ prettyCredential cred
  CertAccountUnregistrationDeposit cred deposit ->
    "Account Unregistration (refund " ++ show deposit ++ "): " ++ prettyCredential cred
  CertDelegationToDRep cred drep ->
    "Delegation to DRep: " ++ prettyCredential cred ++ " -> " ++ prettyDRep drep
  CertDelegationToStakePoolAndDRep cred pool drep ->
    "Delegation to Pool+DRep: " ++ prettyCredential cred
    ++ " -> pool=" ++ prettyHash28 pool ++ ", drep=" ++ prettyDRep drep
  CertAccountRegDelegStakePool cred pool deposit ->
    "Reg+Delegate to Pool (deposit " ++ show deposit ++ "): "
    ++ prettyCredential cred ++ " -> " ++ prettyHash28 pool
  CertAccountRegDelegDRep cred drep deposit ->
    "Reg+Delegate to DRep (deposit " ++ show deposit ++ "): "
    ++ prettyCredential cred ++ " -> " ++ prettyDRep drep
  CertAccountRegDelegStakePoolAndDRep cred pool drep deposit ->
    "Reg+Delegate to Pool+DRep (deposit " ++ show deposit ++ "): "
    ++ prettyCredential cred ++ " -> pool=" ++ prettyHash28 pool ++ ", drep=" ++ prettyDRep drep
  CertCommitteeAuth cold hot ->
    "Committee Auth: cold=" ++ prettyCredential cold ++ " hot=" ++ prettyCredential hot
  CertCommitteeResignation cold anchor ->
    "Committee Resignation: " ++ prettyCredential cold
    ++ maybe "" (\a -> " anchor=" ++ prettyAnchor a) anchor
  CertDRepRegistration cred deposit anchor ->
    "DRep Registration (deposit " ++ show deposit ++ "): " ++ prettyCredential cred
    ++ maybe "" (\a -> " anchor=" ++ prettyAnchor a) anchor
  CertDRepUnregistration cred deposit ->
    "DRep Unregistration (refund " ++ show deposit ++ "): " ++ prettyCredential cred
  CertDRepUpdate cred anchor ->
    "DRep Update: " ++ prettyCredential cred
    ++ maybe "" (\a -> " anchor=" ++ prettyAnchor a) anchor

prettyCredential :: Credential -> String
prettyCredential (CredKeyHash h) = "KeyHash " ++ prettyHash28 h
prettyCredential (CredScriptHash h) = "ScriptHash " ++ prettyHash28 h

prettyDRep :: DRep -> String
prettyDRep (DRepKeyHash h) = "KeyHash " ++ prettyHash28 h
prettyDRep (DRepScriptHash h) = "ScriptHash " ++ prettyHash28 h
prettyDRep DRepAlwaysAbstain = "AlwaysAbstain"
prettyDRep DRepAlwaysNoConfidence = "AlwaysNoConfidence"

prettyAnchor :: Anchor -> String
prettyAnchor Anchor{..} = T.unpack anchorUrl ++ " (" ++ prettyHash32 anchorDataHash ++ ")"

--------------------------------------------------------------------------------
-- Witness set pretty printing
--------------------------------------------------------------------------------

prettyWitnessSet :: Int -> TransactionWitnessSet -> String
prettyWitnessSet idx TransactionWitnessSet{..} =
  unlines $
    [ "Witness Set " ++ show idx
    ]
    ++ (if null twsVKeyWitnesses then []
        else ("  VKey Witnesses: " ++ show (length twsVKeyWitnesses))
             : map prettyVKeyWitness twsVKeyWitnesses)
    ++ (if null twsNativeScripts then []
        else ["  Native Scripts: " ++ show (length twsNativeScripts)])
    ++ (if null twsBootstrapWitnesses then []
        else ["  Bootstrap Witnesses: " ++ show (length twsBootstrapWitnesses)])
    ++ (if null twsPlutusV1Scripts then []
        else ["  Plutus V1 Scripts: " ++ show (length twsPlutusV1Scripts)])
    ++ (if null twsPlutusData then []
        else ["  Plutus Data: " ++ show (length twsPlutusData)])
    ++ (if null twsRedeemers then []
        else ["  Redeemers: " ++ show (length twsRedeemers)])
    ++ (if null twsPlutusV2Scripts then []
        else ["  Plutus V2 Scripts: " ++ show (length twsPlutusV2Scripts)])
    ++ (if null twsPlutusV3Scripts then []
        else ["  Plutus V3 Scripts: " ++ show (length twsPlutusV3Scripts)])

prettyVKeyWitness :: VKeyWitness -> String
prettyVKeyWitness VKeyWitness{..} =
  "    VKey: " ++ prettyVKey vkwVKey ++ "\n"
  ++ "    Sig:  " ++ prettyBS vkwSignature

--------------------------------------------------------------------------------
-- Auxiliary data pretty printing
--------------------------------------------------------------------------------

prettyAuxData :: (TransactionIndex, AuxiliaryData) -> String
prettyAuxData (idx, auxData) =
  unlines $
    [ "Auxiliary Data (Tx " ++ show idx ++ ")"
    ] ++ case auxData of
      AuxMetadata meta ->
        ["  Metadata: " ++ show (Map.size meta) ++ " entries"]
        ++ map (\(k, v) -> "    " ++ show k ++ ": " ++ prettyMetadatum v) (Map.toList meta)
      AuxArray meta scripts ->
        ["  Metadata: " ++ show (Map.size meta) ++ " entries"]
        ++ ["  Native Scripts: " ++ show (length scripts)]
      AuxMap{..} ->
        maybe [] (\m -> ["  Metadata: " ++ show (Map.size m) ++ " entries"]) auxMetadata
        ++ (if null auxNativeScripts then [] else ["  Native Scripts: " ++ show (length auxNativeScripts)])
        ++ (if null auxPlutusV1 then [] else ["  Plutus V1 Scripts: " ++ show (length auxPlutusV1)])
        ++ (if null auxPlutusV2 then [] else ["  Plutus V2 Scripts: " ++ show (length auxPlutusV2)])
        ++ (if null auxPlutusV3 then [] else ["  Plutus V3 Scripts: " ++ show (length auxPlutusV3)])

prettyMetadatum :: Metadatum -> String
prettyMetadatum (MetadatumInt i)    = show i
prettyMetadatum (MetadatumBytes bs) = prettyBS bs
prettyMetadatum (MetadatumText t)   = "\"" ++ T.unpack t ++ "\""
prettyMetadatum (MetadatumList xs)  = "[" ++ intercalate ", " (map prettyMetadatum xs) ++ "]"
prettyMetadatum (MetadatumMap kvs)  = "{" ++ intercalate ", " (map (\(k,v) -> prettyMetadatum k ++ ": " ++ prettyMetadatum v) kvs) ++ "}"
