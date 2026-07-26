# CLAUDE.md - web3-lisp

## Project Overview

Ethereum library implemented in Coalton (a typed language embedded in Common Lisp), modeled after ethers.js and viem. Provides type-safe interfaces for all major Ethereum operations.

## Build & Test

```bash
# Load the full library
sbcl --eval '(asdf:load-system "web3")'

# Load and run all tests (932 tests)
sbcl --non-interactive \
  --eval '(asdf:load-system "web3/tests")' \
  --eval '(web3-tests/runner:run-all-tests)'

# Load a single subsystem
sbcl --eval '(asdf:load-system "web3/types")'
```

The test system is `web3/tests`. The test runner entry point is `web3-tests/runner:run-all-tests`. Tests use a custom `test-case` macro defined in `test/types-tests.lisp`. The `run-all-tests` function lives in `test/erc20-tests.lisp`.

## Architecture

### System Definition

`web3.asd` defines 32 independent ASDF subsystems plus a meta-system `web3` that loads all of them. Each subsystem can be loaded individually (e.g., `web3/types`, `web3/abi`).

### Module Dependency Graph

```
types (Bytes, U256, hex)
├── rlp (RLP encoding/decoding)
├── crypto (keccak256, secp256k1, signatures) [depends: ironclad]
│   └── address (EIP-55 checksum addresses)
│       ├── abi (ABI encode/decode)
│       │   ├── events (event log parsing)
│       │   ├── deploy (CREATE/CREATE2)
│       │   ├── ens (ENS namehash) [depends: uiop]
│       │   │   └── ens-resolver (live ENS resolution via provider)
│       │   ├── multicall (Multicall3 batching)
│       │   ├── eip712 (typed data hashing)
│       │   │   └── permit (EIP-2612 gasless approvals) [depends: provider]
│       │   ├── revert (revert reason decoding: Error, Panic, custom)
│       │   └── abi-parser (Solidity JSON ABI) [depends: cl-json]
│       │       └── contract (high-level contract abstraction)
│       ├── transaction (tx types, encode, decode, sign)
│       │   └── provider (JSON-RPC) [depends: dexador, cl-json, revert]
│       │       ├── wallet (private key + provider)
│       │       ├── erc165 (interface detection, ERC-165)
│       │       ├── erc20, erc721, erc1155 (token standards)
│       │       ├── nonce-manager (multi-address/chain nonce tracking)
│       │       ├── simulate (tx simulation, gas estimation)
│       │       ├── logs (eth_getLogs event log querying)
│       │       └── batch-provider (JSON-RPC batch requests) [depends: dexador, cl-json]
│       └── signature (EIP-191 personal sign, recovery)
│           └── siwe (Sign-In with Ethereum, ERC-4361) [depends: split-sequence]
├── chain (pre-configured networks)
├── units (parseUnits/formatUnits)
├── gas (EIP-1559 fee calculation) [depends: cl-json]
├── block (block/header parsing) [depends: cl-json]
├── receipt (tx receipt parsing) [depends: cl-json]
├── ws-provider (WebSocket subscriptions) [depends: cl-json]
├── blob (EIP-4844 blob encoding) [depends: ironclad]
│   └── kzg (KZG commitments via FFI) [depends: cffi]
└── hdwallet (BIP-39/BIP-32 HD wallets) [depends: ironclad, dexador]
```

### Source Layout

```
src/{module}/package.lisp    -- Coalton package definition with exports
src/{module}/*.lisp          -- Implementation files
test/{module}-tests.lisp     -- CL test file using test-case macro
test/package.lisp            -- Test package definitions (web3-tests, web3-tests/runner)
test/helpers.lisp            -- Coalton test helper functions
```

### Coding Patterns

- **Coalton for core logic**: Type-safe functions using `coalton-toplevel`, `define`, `declare`
- **Common Lisp for test runners**: `defun run-*-tests` functions in CL that call Coalton test helpers
- **Error handling**: `Web3Result` type alias (Coalton `Result` with string errors)
- **Package nicknames**: Modules use `:local-nicknames` (e.g., `#:types` for `#:web3/types`)
- **Readtable**: All Coalton files use `(named-readtables:in-readtable coalton:coalton)`
- **Exports**: Each module's `package.lisp` declares all public symbols

### Key Types

- `Bytes` — wrapper around CL byte vectors (in `web3/types`)
- `U256` — 256-bit unsigned integer (in `web3/types`)
- `Address` — 20-byte Ethereum address (in `web3/address`)
- `RlpItem` — RLP-encodable tree (`RlpBytes` | `RlpList`) (in `web3/rlp`)
- `AbiValue` — ABI-encodable value (`AbiUintVal` | `AbiBoolVal` | ...) (in `web3/abi`)

### External Dependencies

- `coalton` — The Coalton language compiler
- `ironclad` — Cryptographic primitives
- `dexador` — HTTP client
- `cl-json` — JSON parsing
- `cffi` — C FFI for KZG commitments
- `split-sequence` — String splitting
- `named-readtables` — Readtable management

### Related Projects

- [ssz-lisp](https://github.com/samdefmacro/ssz-lisp) — Ethereum SSZ serialization in Coalton with full spec compliance

### Reference Implementations (in `refs/`, git-ignored)

To set up reference repos for local reading:
```bash
git clone --depth 1 https://github.com/ethers-io/ethers.js refs/ethers.js
git clone --depth 1 https://github.com/wevm/viem refs/viem
git clone --depth 1 https://github.com/coalton-lang/coalton.git refs/coalton
```

- `refs/ethers.js/` — ethers-io/ethers.js
- `refs/viem/` — wevm/viem
- `refs/coalton/` — coalton-lang/coalton

## The development loop (warm image, not cold sbcl runs) — cl-agent-repl

```
scripts/dev.sh start          # warm SBCL + Swank image, port 4008 (once; Coalton compile takes minutes first time)
scripts/dev.sh eval '(+ 1 2)' # ~0.1s per eval against the warm image
scripts/dev.sh test rlp       # one module: (web3-tests/runner::run-rlp-tests)
scripts/dev.sh test-all       # full suite; fails if any test fails
scripts/dev.sh docs-check     # PAX doc transcripts (once docs/ manuals exist)
```

Workflow discipline (in order):
1. **Ground before writing**: check symbols exist in the live image —
   `dev.sh eval '(describe (quote web3:get-balance))'`, `(apropos "erc20")`.
   Do not guess APIs.
2. **Develop in small evals**; for Coalton code eval `(coalton:coalton ...)`
   forms through the same channel.
3. **Edit files, then re-load and verify**: `dev.sh eval '(asdf:load-system "web3/tests")'`
   and re-run the relevant module tests. Reload is YOUR job.
4. Coalton type/struct layout changes and `defstruct`/`defconstant` changes
   cannot be hot-patched: restart (`dev.sh stop && dev.sh start`).

Eval contract (scripts/dev-swank-eval.lisp): exit 0 ok / 1 lisp error (with
backtrace frames) / 2 connection error (image down — NOT your code; run
dev.sh start) / 3 timed out and interrupted (default 20s, image survived;
raise DEV_EVAL_TIMEOUT for long forms) / 4 hard hang (restart the image).
Output capped at 10k chars with a TRUNCATED marker. Every eval is logged to
.dev-runtime/swank-dev/eval-metrics.log — do not delete it.

A PostToolUse hook (scripts/paren-hook.sh) checks delimiter balance on every
.lisp/.asd edit and feeds errors straight back — fix them in the same turn.
