# Cardano Conway Block Decoder

A Haskell tool that reads hex-encoded Conway-era CBOR block files from the Cardano blockchain and outputs a human-readable representation. Useful for debugging and inspecting block contents.

## Usage

```bash
# Pretty-printed markdown output
cardano-block-decoder block.cbor

# Structured JSON output
cardano-block-decoder --json block.cbor
```

The input file should contain a single hex-encoded CBOR block.

### Markdown output

```
### Cardano Era 7 Block
**Block Structure**
#### Header Body
- **Block Number**: 4043212
- **Slot**: 83413505
- **Prev Hash**:
  `e60aa1eb682ac55653605fc98d84caaf3b1dc498e01362b3459458d6fee68812`
- **Issuer VKey**:
  `703561fe56f882433613ccc31724d17b2691e2c97e19f7ee206412caaff02b03`
...
- **Protocol Version**: 10.7

#### Transaction 0
- **Inputs**: 1
  - `b45589419cdc...#0`
- **Outputs**: 1
  - Address: `addr_test1qq5yf4nrp...`
  - Value: 9997828691 lovelace
- **Fee**: 171309 lovelace
```

### JSON output

Produces a structured JSON object with full block contents:

```json
{
    "era_code": 7,
    "block": {
        "header": { "header_body": { ... }, "body_signature": "..." },
        "transaction_bodies": [ ... ],
        "transaction_witness_sets": [ ... ],
        "auxiliary_data_set": null,
        "invalid_transactions": []
    }
}
```

## What gets decoded

- **Header**: block number, slot, previous hash, issuer VKey, VRF VKey, VRF certificate, body hash, body size, operational cert, protocol version
- **Transactions**: inputs, outputs (with bech32 addresses), fee, TTL, validity interval, mint/burn, collateral, required signers, network ID, reference inputs
- **Certificates**: stake registration/deregistration, delegation (pool, DRep, pool+DRep), pool registration/retirement, committee auth/resignation, DRep registration/update/unregistration
- **Governance**: voting procedures, proposal procedures (parameter changes, hard forks, treasury withdrawals, no confidence, committee updates, new constitution, info actions)
- **Witnesses**: VKey witnesses, native scripts, bootstrap witnesses, Plutus V1/V2/V3 scripts, Plutus data, redeemers
- **Auxiliary data**: transaction metadata, native scripts, Plutus scripts

Addresses are displayed in bech32 format (`addr`/`addr_test`/`stake`/`stake_test`).
## Building

Requires GHC 9.6 and cabal.

```bash
cd cardano-block-decoder
cabal build
```

## Testing

```bash
cabal test
```

Tests use golden files in `test/golden/` — see `test/golden/README.md` for how to regenerate them after intentional output changes.

## Reference

The CBOR encoding follows the Conway-era block format defined in `conway.cddl`. 
