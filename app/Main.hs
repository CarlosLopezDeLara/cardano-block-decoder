module Main (main) where

import Cardano.Conway.Block
import Cardano.Conway.Json
import Cardano.Conway.Pretty
import Codec.CBOR.Read (deserialiseFromBytes)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL.Char8
import Data.List (partition)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let (jsonFlags, files) = partition (== "--json") args
    case files of
        [filePath] -> do
            hexContent <- BS.readFile filePath
            let cborBytes = Base16.decodeLenient hexContent
            case deserialiseFromBytes decodeBlock (BSL.fromStrict cborBytes) of
                Left err -> putStrLn $ "Error decoding CBOR: " ++ show err
                Right (_, block)
                  | not (null jsonFlags) ->
                      BSL.Char8.putStrLn (Aeson.encodePretty (blockToJSON block))
                  | otherwise ->
                      putStrLn (prettyBlock block)
        _ -> putStrLn "Usage: cardano-block-decoder [--json] <path-to-cbor-file>"
