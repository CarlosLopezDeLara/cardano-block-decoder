{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Use ++" -}
{- HLINT ignore "Use list comprehension" -}
{- HLINT ignore "Avoid lambda" -}

module Cardano.Conway.Json (blockToJSON) where

import Cardano.Conway.Block
import Cardano.Conway.Pretty (prettyAddress)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS.Char8
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

bsToHex :: BS.ByteString -> T.Text
bsToHex = T.pack . BS.Char8.unpack . Base16.encode

hash32ToJSON :: Hash32 -> Aeson.Value
hash32ToJSON (Hash32 bs) = Aeson.String (bsToHex bs)

hash28ToJSON :: Hash28 -> Aeson.Value
hash28ToJSON (Hash28 bs) = Aeson.String (bsToHex bs)

vkeyToJSON :: VKey -> Aeson.Value
vkeyToJSON (VKey bs) = Aeson.String (bsToHex bs)

vrfVKeyToJSON :: VrfVKey -> Aeson.Value
vrfVKeyToJSON (VrfVKey bs) = Aeson.String (bsToHex bs)

addressToJSON :: BS.ByteString -> Aeson.Value
addressToJSON = Aeson.String . T.pack . prettyAddress

--------------------------------------------------------------------------------
-- Top-level
--------------------------------------------------------------------------------

blockToJSON :: Block -> Aeson.Value
blockToJSON Block{..} = Aeson.object
  [ "era_code" .= (7 :: Int)
  , "block" .= Aeson.object
      [ "header" .= headerToJSON bHeader
      , "transaction_bodies" .= map transactionBodyToJSON bTransactionBodies
      , "transaction_witness_sets" .= map witnessSetToJSON bTransactionWitnessSets
      , "auxiliary_data_set" .= auxDataSetToJSON bAuxiliaryDataSet
      , "invalid_transactions" .= bInvalidTransactions
      ]
  ]

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

headerToJSON :: Header -> Aeson.Value
headerToJSON Header{..} = Aeson.object
  [ "header_body" .= headerBodyToJSON hHeaderBody
  , "body_signature" .= bsToHex hBodySignature
  ]

headerBodyToJSON :: HeaderBody -> Aeson.Value
headerBodyToJSON HeaderBody{..} = Aeson.object
  [ "block_number" .= hbBlockNumber
  , "slot" .= show hbSlot
  , "prev_hash" .= maybe Aeson.Null (\h -> hash32ToJSON h) hbPrevHash
  , "issuer_vkey" .= vkeyToJSON hbIssuerVKey
  , "vrf_vkey" .= vrfVKeyToJSON hbVrfVKey
  , "leader_cert" .= vrfCertToJSON hbVrfResult
  , "block_body_size" .= hbBlockBodySize
  , "block_body_hash" .= hash32ToJSON hbBlockBodyHash
  , "operational_cert" .= operationalCertToJSON hbOperationalCert
  , "protocol_version" .= protocolVersionToJSON hbProtocolVersion
  ]

vrfCertToJSON :: VrfCert -> Aeson.Value
vrfCertToJSON (VrfCert output proof) = Aeson.object
  [ "VrfResult" .= Aeson.object
      [ "output" .= map word8ToInt (BS.unpack output)
      , "proof"  .= map word8ToInt (BS.unpack proof)
      ]
  ]
  where
    word8ToInt :: Word8 -> Int
    word8ToInt = Prelude.fromIntegral

operationalCertToJSON :: OperationalCert -> Aeson.Value
operationalCertToJSON OperationalCert{..} = Aeson.object
  [ "hot_vkey" .= vkeyToJSON opCertHotVKey
  , "sequence_number" .= opCertSeqNum
  , "kes_period" .= opCertKesPeriod
  , "sigma" .= bsToHex opCertSigma
  ]

protocolVersionToJSON :: ProtocolVersion -> Aeson.Value
protocolVersionToJSON ProtocolVersion{..} = Aeson.object
  [ "major" .= pvMajor
  , "minor" .= pvMinor
  ]

--------------------------------------------------------------------------------
-- Transaction Body
--------------------------------------------------------------------------------

transactionBodyToJSON :: TransactionBody -> Aeson.Value
transactionBodyToJSON TransactionBody{..} = Aeson.object $ concat
  [ [ "inputs" .= map txInputToJSON tbInputs
    , "outputs" .= map txOutputToJSON tbOutputs
    , "fee" .= tbFee
    ]
  , maybe [] (\t -> ["ttl" .= t]) tbTtl
  , if null tbCertificates then []
    else ["certificates" .= map certificateToJSON tbCertificates]
  , if Map.null tbWithdrawals then []
    else ["withdrawals" .= Aeson.object
            (map (\(addr, coin) -> Key.fromText (T.pack (prettyAddress addr)) .= coin)
                 (Map.toList tbWithdrawals))]
  , maybe [] (\h -> ["auxiliary_data_hash" .= hash32ToJSON h]) tbAuxiliaryDataHash
  , maybe [] (\s -> ["validity_start" .= s]) tbValidityStart
  , if Map.null tbMint then []
    else ["mint" .= multiAssetToJSON tbMint]
  , maybe [] (\h -> ["script_data_hash" .= hash32ToJSON h]) tbScriptDataHash
  , if null tbCollateral then []
    else ["collateral" .= map txInputToJSON tbCollateral]
  , if null tbRequiredSigners then []
    else ["required_signers" .= map hash28ToJSON tbRequiredSigners]
  , maybe [] (\nid -> ["network_id" .= nid]) tbNetworkId
  , maybe [] (\o -> ["collateral_return" .= txOutputToJSON o]) tbCollateralReturn
  , maybe [] (\c -> ["total_collateral" .= c]) tbTotalCollateral
  , if null tbReferenceInputs then []
    else ["reference_inputs" .= map txInputToJSON tbReferenceInputs]
  , maybe [] (\v -> ["voting_procedures" .= votingProceduresToJSON v]) tbVotingProcedures
  , if null tbProposalProcedures then []
    else ["proposal_procedures" .= map proposalToJSON tbProposalProcedures]
  , maybe [] (\t -> ["treasury_value" .= t]) tbTreasuryValue
  , maybe [] (\d -> ["donation" .= d]) tbDonation
  ]

txInputToJSON :: TransactionInput -> Aeson.Value
txInputToJSON TransactionInput{..} = Aeson.object
  [ "transaction_id" .= hash32ToJSON txInputId
  , "index" .= txInputIndex
  ]

txOutputToJSON :: TransactionOutput -> Aeson.Value
txOutputToJSON TransactionOutput{..} = Aeson.object $ concat
  [ [ "address" .= addressToJSON txOutAddress
    , "value" .= valueToJSON txOutValue
    ]
  , maybe [] (\h -> ["datum_hash" .= hash32ToJSON h]) txOutDatumHash
  , maybe [] (\d -> ["datum" .= datumOptionToJSON d]) txOutDatum
  , maybe [] (\s -> ["script_ref" .= bsToHex s]) txOutScriptRef
  ]

valueToJSON :: Value -> Aeson.Value
valueToJSON (ValueLovelace coin) = Aeson.object ["lovelace" .= coin]
valueToJSON (ValueMultiAsset coin assets) = Aeson.object $ concat
  [ ["lovelace" .= coin]
  , if Map.null assets then []
    else ["assets" .= multiAssetToJSON assets]
  ]

multiAssetToJSON :: Map.Map PolicyId (Map.Map AssetName Integer) -> Aeson.Value
multiAssetToJSON assets = Aeson.object
  [ Key.fromText (bsToHex (unHash28 pid)) .= Aeson.object
      (map (\(name, amt) -> Key.fromText (bsToHex name) .= amt)
           (Map.toList assetMap))
  | (pid, assetMap) <- Map.toList assets
  ]
  where
    unHash28 (Hash28 bs) = bs

datumOptionToJSON :: DatumOption -> Aeson.Value
datumOptionToJSON (DatumHash h) = Aeson.object
  [ "type" .= ("hash" :: T.Text)
  , "hash" .= hash32ToJSON h
  ]
datumOptionToJSON (DatumInline bs) = Aeson.object
  [ "type" .= ("inline" :: T.Text)
  , "bytes" .= bsToHex bs
  ]

--------------------------------------------------------------------------------
-- Certificates
--------------------------------------------------------------------------------

certificateToJSON :: Certificate -> Aeson.Value
certificateToJSON cert = case cert of
  CertAccountRegistration cred -> Aeson.object
    [ "type" .= ("account_registration" :: T.Text)
    , "credential" .= credentialToJSON cred
    ]
  CertAccountUnregistration cred -> Aeson.object
    [ "type" .= ("account_unregistration" :: T.Text)
    , "credential" .= credentialToJSON cred
    ]
  CertDelegationToStakePool cred pool -> Aeson.object
    [ "type" .= ("delegation_to_stake_pool" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "pool" .= hash28ToJSON pool
    ]
  CertPoolRegistration pp -> Aeson.object
    [ "type" .= ("pool_registration" :: T.Text)
    , "params" .= poolParamsToJSON pp
    ]
  CertPoolRetirement pool epoch -> Aeson.object
    [ "type" .= ("pool_retirement" :: T.Text)
    , "pool" .= hash28ToJSON pool
    , "epoch" .= epoch
    ]
  CertAccountRegistrationDeposit cred deposit -> Aeson.object
    [ "type" .= ("account_registration_deposit" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "deposit" .= deposit
    ]
  CertAccountUnregistrationDeposit cred deposit -> Aeson.object
    [ "type" .= ("account_unregistration_deposit" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "deposit" .= deposit
    ]
  CertDelegationToDRep cred drep -> Aeson.object
    [ "type" .= ("delegation_to_drep" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "drep" .= drepToJSON drep
    ]
  CertDelegationToStakePoolAndDRep cred pool drep -> Aeson.object
    [ "type" .= ("delegation_to_stake_pool_and_drep" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "pool" .= hash28ToJSON pool
    , "drep" .= drepToJSON drep
    ]
  CertAccountRegDelegStakePool cred pool deposit -> Aeson.object
    [ "type" .= ("reg_deleg_stake_pool" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "pool" .= hash28ToJSON pool
    , "deposit" .= deposit
    ]
  CertAccountRegDelegDRep cred drep deposit -> Aeson.object
    [ "type" .= ("reg_deleg_drep" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "drep" .= drepToJSON drep
    , "deposit" .= deposit
    ]
  CertAccountRegDelegStakePoolAndDRep cred pool drep deposit -> Aeson.object
    [ "type" .= ("reg_deleg_stake_pool_and_drep" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "pool" .= hash28ToJSON pool
    , "drep" .= drepToJSON drep
    , "deposit" .= deposit
    ]
  CertCommitteeAuth cold hot -> Aeson.object
    [ "type" .= ("committee_auth" :: T.Text)
    , "cold" .= credentialToJSON cold
    , "hot" .= credentialToJSON hot
    ]
  CertCommitteeResignation cold anchor -> Aeson.object $ concat
    [ [ "type" .= ("committee_resignation" :: T.Text)
      , "cold" .= credentialToJSON cold
      ]
    , maybe [] (\a -> ["anchor" .= anchorToJSON a]) anchor
    ]
  CertDRepRegistration cred deposit anchor -> Aeson.object $ concat
    [ [ "type" .= ("drep_registration" :: T.Text)
      , "credential" .= credentialToJSON cred
      , "deposit" .= deposit
      ]
    , maybe [] (\a -> ["anchor" .= anchorToJSON a]) anchor
    ]
  CertDRepUnregistration cred deposit -> Aeson.object
    [ "type" .= ("drep_unregistration" :: T.Text)
    , "credential" .= credentialToJSON cred
    , "deposit" .= deposit
    ]
  CertDRepUpdate cred anchor -> Aeson.object $ concat
    [ [ "type" .= ("drep_update" :: T.Text)
      , "credential" .= credentialToJSON cred
      ]
    , maybe [] (\a -> ["anchor" .= anchorToJSON a]) anchor
    ]

credentialToJSON :: Credential -> Aeson.Value
credentialToJSON (CredKeyHash h) = Aeson.object
  [ "type" .= ("key_hash" :: T.Text)
  , "hash" .= hash28ToJSON h
  ]
credentialToJSON (CredScriptHash h) = Aeson.object
  [ "type" .= ("script_hash" :: T.Text)
  , "hash" .= hash28ToJSON h
  ]

drepToJSON :: DRep -> Aeson.Value
drepToJSON (DRepKeyHash h) = Aeson.object
  [ "type" .= ("key_hash" :: T.Text)
  , "hash" .= hash28ToJSON h
  ]
drepToJSON (DRepScriptHash h) = Aeson.object
  [ "type" .= ("script_hash" :: T.Text)
  , "hash" .= hash28ToJSON h
  ]
drepToJSON DRepAlwaysAbstain = Aeson.String "always_abstain"
drepToJSON DRepAlwaysNoConfidence = Aeson.String "always_no_confidence"

anchorToJSON :: Anchor -> Aeson.Value
anchorToJSON Anchor{..} = Aeson.object
  [ "url" .= anchorUrl
  , "data_hash" .= hash32ToJSON anchorDataHash
  ]

poolParamsToJSON :: PoolParams -> Aeson.Value
poolParamsToJSON PoolParams{..} = Aeson.object $ concat
  [ [ "operator" .= hash28ToJSON ppOperator
    , "vrf_keyhash" .= hash32ToJSON ppVrfKeyhash
    , "pledge" .= ppPledge
    , "cost" .= ppCost
    , "margin" .= Aeson.object
        [ "numerator" .= fst ppMargin
        , "denominator" .= snd ppMargin
        ]
    , "reward_account" .= addressToJSON ppRewardAccount
    , "pool_owners" .= map hash28ToJSON ppPoolOwners
    , "relays" .= map relayToJSON ppRelays
    ]
  , maybe [] (\m -> ["pool_metadata" .= poolMetadataToJSON m]) ppPoolMetadata
  ]

relayToJSON :: Relay -> Aeson.Value
relayToJSON (SingleHostAddr port ipv4 ipv6) = Aeson.object $ concat
  [ ["type" .= ("single_host_addr" :: T.Text)]
  , maybe [] (\p -> ["port" .= p]) port
  , maybe [] (\ip -> ["ipv4" .= bsToHex ip]) ipv4
  , maybe [] (\ip -> ["ipv6" .= bsToHex ip]) ipv6
  ]
relayToJSON (SingleHostName port dns) = Aeson.object $ concat
  [ [ "type" .= ("single_host_name" :: T.Text)
    , "dns_name" .= dns
    ]
  , maybe [] (\p -> ["port" .= p]) port
  ]
relayToJSON (MultiHostName dns) = Aeson.object
  [ "type" .= ("multi_host_name" :: T.Text)
  , "dns_name" .= dns
  ]

poolMetadataToJSON :: PoolMetadata -> Aeson.Value
poolMetadataToJSON PoolMetadata{..} = Aeson.object
  [ "url" .= pmUrl
  , "hash" .= bsToHex pmHash
  ]

--------------------------------------------------------------------------------
-- Voting / Governance
--------------------------------------------------------------------------------

votingProceduresToJSON :: VotingProcedures -> Aeson.Value
votingProceduresToJSON vps = Aeson.toJSON
  [ Aeson.object
      [ "voter" .= voterToJSON voter
      , "votes" .= [ Aeson.object
                        [ "gov_action_id" .= govActionIdToJSON gaid
                        , "procedure" .= votingProcedureToJSON vp
                        ]
                    | (gaid, vp) <- Map.toList votes
                    ]
      ]
  | (voter, votes) <- Map.toList vps
  ]

voterToJSON :: Voter -> Aeson.Value
voterToJSON v = case v of
  VoterCommitteeKeyHash h -> Aeson.object
    ["type" .= ("committee_key_hash" :: T.Text), "hash" .= hash28ToJSON h]
  VoterCommitteeScriptHash h -> Aeson.object
    ["type" .= ("committee_script_hash" :: T.Text), "hash" .= hash28ToJSON h]
  VoterDRepKeyHash h -> Aeson.object
    ["type" .= ("drep_key_hash" :: T.Text), "hash" .= hash28ToJSON h]
  VoterDRepScriptHash h -> Aeson.object
    ["type" .= ("drep_script_hash" :: T.Text), "hash" .= hash28ToJSON h]
  VoterStakePoolKeyHash h -> Aeson.object
    ["type" .= ("stake_pool_key_hash" :: T.Text), "hash" .= hash28ToJSON h]

govActionIdToJSON :: GovActionId -> Aeson.Value
govActionIdToJSON GovActionId{..} = Aeson.object
  [ "transaction_id" .= hash32ToJSON gaiTxId
  , "index" .= gaiIndex
  ]

votingProcedureToJSON :: VotingProcedure -> Aeson.Value
votingProcedureToJSON VotingProcedure{..} = Aeson.object $ concat
  [ ["vote" .= vpVote]
  , maybe [] (\a -> ["anchor" .= anchorToJSON a]) vpAnchor
  ]

proposalToJSON :: ProposalProcedure -> Aeson.Value
proposalToJSON ProposalProcedure{..} = Aeson.object
  [ "deposit" .= propDeposit
  , "reward_account" .= addressToJSON propRewardAccount
  , "gov_action" .= govActionToJSON propGovAction
  , "anchor" .= anchorToJSON propAnchor
  ]

govActionToJSON :: GovAction -> Aeson.Value
govActionToJSON act = case act of
  GovActionParameterChange prevId _ppUpdate guardrail -> Aeson.object $ concat
    [ ["type" .= ("parameter_change" :: T.Text)]
    , maybe [] (\p -> ["prev_gov_action_id" .= govActionIdToJSON p]) prevId
    , maybe [] (\g -> ["guardrail_script" .= hash28ToJSON g]) guardrail
    ]
  GovActionHardFork prevId pv -> Aeson.object $ concat
    [ [ "type" .= ("hard_fork" :: T.Text)
      , "protocol_version" .= protocolVersionToJSON pv
      ]
    , maybe [] (\p -> ["prev_gov_action_id" .= govActionIdToJSON p]) prevId
    ]
  GovActionTreasuryWithdrawals withdrawals guardrail -> Aeson.object $ concat
    [ [ "type" .= ("treasury_withdrawals" :: T.Text)
      , "withdrawals" .= Aeson.object
          (map (\(addr, coin) -> Key.fromText (T.pack (prettyAddress addr)) .= coin)
               (Map.toList withdrawals))
      ]
    , maybe [] (\g -> ["guardrail_script" .= hash28ToJSON g]) guardrail
    ]
  GovActionNoConfidence prevId -> Aeson.object $ concat
    [ ["type" .= ("no_confidence" :: T.Text)]
    , maybe [] (\p -> ["prev_gov_action_id" .= govActionIdToJSON p]) prevId
    ]
  GovActionUpdateCommittee prevId removals additions threshold -> Aeson.object $ concat
    [ [ "type" .= ("update_committee" :: T.Text)
      , "removals" .= map credentialToJSON removals
      , "additions" .= Aeson.toJSON
          [ Aeson.object ["credential" .= credentialToJSON c, "epoch" .= e]
          | (c, e) <- Map.toList additions
          ]
      , "threshold" .= Aeson.object
          [ "numerator" .= fst threshold
          , "denominator" .= snd threshold
          ]
      ]
    , maybe [] (\p -> ["prev_gov_action_id" .= govActionIdToJSON p]) prevId
    ]
  GovActionNewConstitution prevId anchor guardrail -> Aeson.object $ concat
    [ [ "type" .= ("new_constitution" :: T.Text)
      , "anchor" .= anchorToJSON anchor
      ]
    , maybe [] (\p -> ["prev_gov_action_id" .= govActionIdToJSON p]) prevId
    , maybe [] (\g -> ["guardrail_script" .= hash28ToJSON g]) guardrail
    ]
  GovActionInfo -> Aeson.object
    [ "type" .= ("info" :: T.Text)
    ]

--------------------------------------------------------------------------------
-- Witness Set
--------------------------------------------------------------------------------

witnessSetToJSON :: TransactionWitnessSet -> Aeson.Value
witnessSetToJSON TransactionWitnessSet{..} = Aeson.object $ concat
  [ if null twsVKeyWitnesses then []
    else ["vkey_witnesses" .= map vkeyWitnessToJSON twsVKeyWitnesses]
  , if null twsNativeScripts then []
    else ["native_scripts" .= map nativeScriptToJSON twsNativeScripts]
  , if null twsBootstrapWitnesses then []
    else ["bootstrap_witnesses" .= map bootstrapWitnessToJSON twsBootstrapWitnesses]
  , if null twsPlutusV1Scripts then []
    else ["plutus_v1_scripts" .= map bsToHex twsPlutusV1Scripts]
  , if null twsPlutusData then []
    else ["plutus_data" .= map plutusDataToJSON twsPlutusData]
  , if null twsRedeemers then []
    else ["redeemers" .= map redeemerToJSON twsRedeemers]
  , if null twsPlutusV2Scripts then []
    else ["plutus_v2_scripts" .= map bsToHex twsPlutusV2Scripts]
  , if null twsPlutusV3Scripts then []
    else ["plutus_v3_scripts" .= map bsToHex twsPlutusV3Scripts]
  ]

vkeyWitnessToJSON :: VKeyWitness -> Aeson.Value
vkeyWitnessToJSON VKeyWitness{..} = Aeson.object
  [ "vkey" .= vkeyToJSON vkwVKey
  , "signature" .= bsToHex vkwSignature
  ]

bootstrapWitnessToJSON :: BootstrapWitness -> Aeson.Value
bootstrapWitnessToJSON BootstrapWitness{..} = Aeson.object
  [ "public_key" .= vkeyToJSON bwPublicKey
  , "signature" .= bsToHex bwSignature
  , "chain_code" .= bsToHex bwChainCode
  , "attributes" .= bsToHex bwAttributes
  ]

nativeScriptToJSON :: NativeScript -> Aeson.Value
nativeScriptToJSON ns = case ns of
  ScriptPubkey h -> Aeson.object
    [ "type" .= ("sig" :: T.Text), "key_hash" .= hash28ToJSON h ]
  ScriptAll scripts -> Aeson.object
    [ "type" .= ("all" :: T.Text), "scripts" .= map nativeScriptToJSON scripts ]
  ScriptAny scripts -> Aeson.object
    [ "type" .= ("any" :: T.Text), "scripts" .= map nativeScriptToJSON scripts ]
  ScriptNOfK n scripts -> Aeson.object
    [ "type" .= ("n_of_k" :: T.Text), "n" .= n, "scripts" .= map nativeScriptToJSON scripts ]
  ScriptInvalidBefore slot -> Aeson.object
    [ "type" .= ("invalid_before" :: T.Text), "slot" .= slot ]
  ScriptInvalidHereafter slot -> Aeson.object
    [ "type" .= ("invalid_hereafter" :: T.Text), "slot" .= slot ]

plutusDataToJSON :: PlutusData -> Aeson.Value
plutusDataToJSON pd = case pd of
  PlutusConstr _tag fields -> Aeson.object
    [ "type" .= ("constr" :: T.Text)
    , "alternative" .= pcfTag fields
    , "fields" .= map plutusDataToJSON (pcfFields fields)
    ]
  PlutusMap pairs -> Aeson.object
    [ "type" .= ("map" :: T.Text)
    , "entries" .= [ Aeson.object ["k" .= plutusDataToJSON k, "v" .= plutusDataToJSON v]
                    | (k, v) <- pairs
                    ]
    ]
  PlutusList xs -> Aeson.object
    [ "type" .= ("list" :: T.Text)
    , "items" .= map plutusDataToJSON xs
    ]
  PlutusInteger i -> Aeson.object
    [ "type" .= ("int" :: T.Text)
    , "value" .= i
    ]
  PlutusBytes bs -> Aeson.object
    [ "type" .= ("bytes" :: T.Text)
    , "value" .= bsToHex bs
    ]
  PlutusBigUInt bs -> Aeson.object
    [ "type" .= ("big_uint" :: T.Text)
    , "value" .= bsToHex bs
    ]
  PlutusBigNInt bs -> Aeson.object
    [ "type" .= ("big_nint" :: T.Text)
    , "value" .= bsToHex bs
    ]

redeemerToJSON :: Redeemer -> Aeson.Value
redeemerToJSON Redeemer{..} = Aeson.object
  [ "tag" .= rdTag
  , "index" .= rdIndex
  , "data" .= plutusDataToJSON rdData
  , "ex_units" .= Aeson.object
      [ "mem" .= exMem rdExUnits
      , "steps" .= exSteps rdExUnits
      ]
  ]

--------------------------------------------------------------------------------
-- Auxiliary Data
--------------------------------------------------------------------------------

auxDataSetToJSON :: Map.Map TransactionIndex AuxiliaryData -> Aeson.Value
auxDataSetToJSON m
  | Map.null m = Aeson.Null
  | otherwise  = Aeson.object
      [ Key.fromText (T.pack (show idx)) .= auxiliaryDataToJSON ad
      | (idx, ad) <- Map.toList m
      ]

auxiliaryDataToJSON :: AuxiliaryData -> Aeson.Value
auxiliaryDataToJSON ad = case ad of
  AuxMetadata meta -> Aeson.object
    [ "metadata" .= metadataToJSON meta ]
  AuxArray meta scripts -> Aeson.object
    [ "metadata" .= metadataToJSON meta
    , "native_scripts" .= map nativeScriptToJSON scripts
    ]
  AuxMap{..} -> Aeson.object $ concat
    [ maybe [] (\m -> ["metadata" .= metadataToJSON m]) auxMetadata
    , if null auxNativeScripts then []
      else ["native_scripts" .= map nativeScriptToJSON auxNativeScripts]
    , if null auxPlutusV1 then []
      else ["plutus_v1_scripts" .= map bsToHex auxPlutusV1]
    , if null auxPlutusV2 then []
      else ["plutus_v2_scripts" .= map bsToHex auxPlutusV2]
    , if null auxPlutusV3 then []
      else ["plutus_v3_scripts" .= map bsToHex auxPlutusV3]
    ]

metadataToJSON :: Metadata -> Aeson.Value
metadataToJSON meta = Aeson.object
  [ Key.fromText (T.pack (show k)) .= metadatumToJSON v
  | (k, v) <- Map.toList meta
  ]

metadatumToJSON :: Metadatum -> Aeson.Value
metadatumToJSON m = case m of
  MetadatumInt i    -> Aeson.toJSON i
  MetadatumBytes bs -> Aeson.object ["bytes" .= bsToHex bs]
  MetadatumText t   -> Aeson.String t
  MetadatumList xs  -> Aeson.toJSON (map metadatumToJSON xs)
  MetadatumMap kvs  -> Aeson.toJSON
    [ Aeson.object ["k" .= metadatumToJSON k, "v" .= metadatumToJSON v]
    | (k, v) <- kvs
    ]
