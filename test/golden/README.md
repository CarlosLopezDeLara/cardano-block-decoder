# Golden Test Files

These files contain the expected output of the block decoder for each test block.
Tests in `test/Spec.hs` compare the decoder's actual output against these files.

## Regenerating after intentional changes

After modifying the pretty-printer or JSON output, rebuild the golden files:

```bash
cd cardano-block-decoder

cabal -v0 run cardano-block-decoder -- ../block.cbor > test/golden/block.pretty.md
cabal -v0 run cardano-block-decoder -- ../sancho.block.cbor > test/golden/sancho.pretty.md

# JSON
cabal -v0 run cardano-block-decoder -- --json ../block.cbor > test/golden/block.json
cabal -v0 run cardano-block-decoder -- --json ../sancho.block.cbor > test/golden/sancho.json
```

Then review the diffs (`git diff test/golden/`) to confirm the changes are expected before committing.
