{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Conway.Block
import Cardano.Conway.Json
import Cardano.Conway.Pretty
import Codec.CBOR.Read (deserialiseFromBytes)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import Test.Hspec

-- | Decode a block from a hex-encoded CBOR file, or fail with a message.
decodeBlockFile :: FilePath -> IO Block
decodeBlockFile path = do
  hexContent <- BS.readFile path
  let cborBytes = Base16.decodeLenient hexContent
  case deserialiseFromBytes decodeBlock (BSL.fromStrict cborBytes) of
    Left err -> fail $ "Error decoding CBOR: " ++ show err
    Right (_, block) -> return block

main :: IO ()
main = hspec $ do
  describe "block.cbor (no transactions)" $ do
    it "pretty-prints to match golden file" $ do
      block <- decodeBlockFile "../block.cbor"
      golden <- readFile "test/golden/block.pretty.md"
      (prettyBlock block ++ "\n") `shouldBe` golden

    it "JSON output matches golden file" $ do
      block <- decodeBlockFile "../block.cbor"
      goldenBS <- BSL.readFile "test/golden/block.json"
      case Aeson.decode goldenBS of
        Nothing -> fail "Could not parse golden JSON file"
        Just goldenVal -> blockToJSON block `shouldBe` goldenVal

  describe "sancho.block.cbor (1 transaction)" $ do
    it "pretty-prints to match golden file" $ do
      block <- decodeBlockFile "../sancho.block.cbor"
      golden <- readFile "test/golden/sancho.pretty.md"
      (prettyBlock block ++ "\n") `shouldBe` golden

    it "JSON output matches golden file" $ do
      block <- decodeBlockFile "../sancho.block.cbor"
      goldenBS <- BSL.readFile "test/golden/sancho.json"
      case Aeson.decode goldenBS of
        Nothing -> fail "Could not parse golden JSON file"
        Just goldenVal -> blockToJSON block `shouldBe` goldenVal
