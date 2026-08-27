# web3-lisp

Ethereum library for Common Lisp — typed core in [Coalton](https://github.com/coalton-lang/coalton), one-liner CL surface on top. Modeled after [ethers.js](https://github.com/ethers-io/ethers.js) and [viem](https://github.com/wevm/viem).

```lisp
(asdf:load-system "web3")

;; Read a balance from the REPL — no Coalton needed
(web3:get-balance "https://eth.llamarpc.com"
                  "0xd8da6bf26964af9d7eed9e03e53415d37aa96045")
;; => 6739821000000000000  (wei, as a CL integer)

;; Bind an ERC-20 contract to typed CL functions
(web3:defcontract usdc
  :address "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
  :abi-file "USDC.json")

(usdc-symbol "https://eth.llamarpc.com")            ; => "USDC"
(usdc-balance-of "https://eth.llamarpc.com" addr)   ; => integer

;; Build calldata for write functions
(usdc-transfer-data "0xrecipient..." 1000000)       ; => "0xa9059cbb..."
```

Three layers, pick the one that fits:

| Layer        | Package         | Use case                                                          |
|--------------|-----------------|-------------------------------------------------------------------|
| **Easy**     | `web3` (CL)     | One-liners from CL/REPL. Strings/integers in, strings/integers out, conditions on errors. |
| **Prelude**  | `web3/prelude`  | Curated re-exports for Coalton consumers — one import for the common surface. |
| **Typed**    | `web3/types`, `web3/provider`, `web3/wallet`, `web3/erc20`, ... | Full Coalton API; reach for it when you want the type safety. |

## Installation

Requires [SBCL](http://www.sbcl.org/) and [Quicklisp](https://www.quicklisp.org/).
The code targets the Coalton shipped in the Quicklisp **2025-06-22** dist; a
newer Coalton master renames internals the library reaches into (see
[Development](#development)).

```lisp
(asdf:load-system "web3")              ; everything
(asdf:load-system "web3/easy")         ; CL convenience only
(asdf:load-system "web3/prelude")      ; Coalton prelude only
(asdf:load-system "web3/provider")     ; or any individual subsystem
```

## CL one-liners (`web3:` package)

```lisp
;; Reads
(web3:get-block-number url)
(web3:chain-id url)
(web3:get-balance url addr-hex)               ; -> integer wei
(web3:get-transaction-count url addr-hex)     ; -> integer nonce
(web3:gas-price url)
(web3:max-priority-fee url)
(web3:get-code url addr-hex)                  ; -> 0x...hex
(web3:eth-call url to-hex data-hex)
(web3:get-receipt url tx-hash)                ; nil if not yet mined
(web3:wait-for-receipt url tx-hash :max-attempts 60 :poll-interval-ms 2000)

;; Units
(web3:parse-ether "1.5")           ; => 1500000000000000000
(web3:format-ether 1500000000000000000)  ; => "1.5"
(web3:parse-gwei "30")
(web3:parse-units "100.50" 6)      ; USDC has 6 decimals

;; Address + hashing
(web3:checksum-address "0xd8da...")     ; -> EIP-55 mixed-case
(web3:keccak256 "0x")                   ; -> 0xc5d2460186...

;; ERC-20 reads
(web3:erc20-name url usdc-address)
(web3:erc20-balance url usdc-address holder-address)

;; Wallets
(let ((w (web3:make-wallet-from-hex "0x<private-key>" url)))
  (web3:wallet-address w)
  (web3:wallet-balance w)
  (web3:wallet-send-eth w "0xrecipient..." (web3:parse-ether "0.1")
                        :chain-id 1))
```

Errors signal `web3:web3-error`:

```lisp
(handler-case (web3:checksum-address "not-hex")
  (web3:web3-error (c)
    (format t "got: ~A" (web3:web3-error-message c))))
```

### Multicall (batched reads)

Multicall3 lets you batch many `eth_call`s into a single round-trip. `web3:multicall` accepts a list of plists and returns one result per call:

```lisp
(web3:multicall url
  (list (list :to usdc-address :data (usdc-balance-of-data holder))
        (list :to weth-address :data (weth-balance-of-data holder))))
;; => ((:success t :data "0x...balance1")
;;     (:success t :data "0x...balance2"))
```

Default `:allow-failure` is `t` (one bad call doesn't fail the batch). Override per-call with `:allow-failure` inside the entry.

### Block reads

`web3:get-block` returns a CL plist instead of raw JSON:

```lisp
(web3:get-block url :latest)
;; => (:number 19345672 :hash "0x..." :parent-hash "0x..."
;;     :timestamp 1717593600 :miner "0x..." :gas-limit 30000000
;;     :gas-used 14523456 :base-fee 12345678901
;;     :transactions-count 142 :size 78912)
```

The tag accepts an integer block number, a keyword (`:latest`, `:earliest`, `:pending`, `:finalized`, `:safe`), or a hex string. Returns NIL if the block doesn't exist.

### Retry + fallback

Wrap any URL-based call with `web3:with-fallback` to get per-URL retries (with exponential backoff) and a fallback chain across multiple endpoints:

```lisp
(web3:with-fallback
  '("https://primary-rpc.example" "https://backup-rpc.example")
  (lambda (url) (web3:get-balance url addr-hex)))
```

The default retryable predicate matches HTTP 5xx, 429, timeouts, and connection refusals; permanent errors (e.g. malformed input) short-circuit so you don't burn the retry budget on bugs in your own code.

## `defcontract` — ABI to CL functions

`defcontract` parses a Solidity ABI at macroexpand time and generates one CL wrapper per function. View/pure functions take a JSON-RPC URL and return ordinary CL values; non-view functions get a `*-data` builder that returns calldata you can send through `wallet-send-transaction`.

```lisp
(web3:defcontract usdc
  :address "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
  :abi-file "USDC.json")  ; or :abi "[ {...} ]"

;; view/pure
(usdc-name url)                       ; -> "USD Coin"
(usdc-decimals url)                   ; -> 6
(usdc-balance-of url holder-hex)      ; -> integer
(usdc-allowance url owner spender)    ; -> integer

;; nonpayable / payable -> calldata builder
(usdc-transfer-data recipient 1000000)
(usdc-approve-data spender 1000000)
```

Supported types: `uint*`, `int*`, `address`, `bool`, `string`, `bytes`, `bytesN`, plus arrays of any of the above (`T[]` and `T[N]`). Solidity tuples (`struct` returns) currently fall through to the typed `web3/contract` API — `defcontract` skips them with a compile-time warning rather than silently mis-encoding.

For each ABI **event**, `defcontract` also generates:

```lisp
(usdc-event-transfer-topic)               ; -> "0xddf252ad..." (topic0)
(usdc-event-transfer topics-hex data-hex) ; -> (:from "0x..." :to "0x..." :value 1500)
```

The decoder takes the topics list (including topic0) and data hex from a log entry and returns a plist of named fields. Indexed and non-indexed inputs are interleaved in declaration order, exactly as the ABI specifies.

## Coalton prelude (typed)

```lisp
(defpackage #:my-app
  (:use #:coalton #:coalton-prelude #:web3/prelude))
(in-package #:my-app)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define provider (make-http-provider "https://eth.llamarpc.com"))

  (define (read-balance addr-hex)
    (do (addr <- (address-from-hex addr-hex))
        (eth-get-balance provider addr))))
```

The prelude re-exports the high-traffic surface across `web3/types`, `web3/address`, `web3/units`, `web3/chain`, `web3/provider`, `web3/transaction`, `web3/wallet`, `web3/erc20`, `web3/abi`, `web3/contract`, and `web3/contract-write`. Everything not in the prelude is available by importing the dedicated module.

## Modules

42 independent ASDF subsystems plus a meta-system `web3` that loads them all.

| Module | Description |
|--------|-------------|
| `web3/easy` | Plain-CL convenience layer (`web3:` package) + `defcontract` |
| `web3/prelude` | Curated Coalton re-exports — one import for the common surface |
| `web3/types` | Core types: `Bytes`, `U256`, hex encoding, `Web3Error`/`Web3Result` |
| `web3/rlp` | RLP encoding/decoding |
| `web3/crypto` | keccak256, secp256k1, ECDSA signatures |
| `web3/address` | Ethereum addresses with EIP-55 checksums |
| `web3/abi` | ABI encoding/decoding (uint, bool, address, bytes, string, arrays, tuples), `abi-encode-packed` |
| `web3/abi-parser` | Parse Solidity JSON ABI files |
| `web3/transaction` | Transaction types (legacy, EIP-2930, EIP-1559, EIP-4844), encoding, signing |
| `web3/provider` | JSON-RPC HTTP provider with 18+ methods |
| `web3/wallet` | Private key wallet with signing and sending |
| `web3/contract` | High-level contract abstraction from ABI JSON |
| `web3/contract-write` | High-level send-transaction for contracts |
| `web3/erc20` | ERC-20 token standard |
| `web3/erc721`/`erc721-metadata` | ERC-721 NFT standard |
| `web3/erc1155`/`erc1155-metadata` | ERC-1155 multi-token standard |
| `web3/events` | Event log parsing and decoding |
| `web3/logs` | Event log querying via `eth_getLogs` |
| `web3/ens` | ENS namehash (EIP-137) |
| `web3/ens-resolver` | Live ENS resolution via provider (forward + reverse with confirmation) |
| `web3/deploy` | CREATE/CREATE2 address computation |
| `web3/multicall` | Multicall3 batching |
| `web3/eip712` | EIP-712 typed data hashing and signing |
| `web3/permit` | EIP-2612 gasless approvals |
| `web3/signature` | EIP-191 personal sign and recovery |
| `web3/siwe` | Sign-In with Ethereum (ERC-4361) |
| `web3/hdwallet` | BIP-39 mnemonics + BIP-32 key derivation |
| `web3/ws-provider` | WebSocket subscriptions (newHeads, logs, syncing, pendingTxs) |
| `web3/gas` | EIP-1559 fee calculation |
| `web3/simulate` | Transaction simulation, gas estimation, access lists |
| `web3/nonce-manager` | Multi-address/chain nonce tracking |
| `web3/receipt` | Transaction receipt parsing |
| `web3/block` | Block and header parsing |
| `web3/chain` | Pre-configured network settings |
| `web3/units` | `parseUnits`/`formatUnits` |
| `web3/blob`, `web3/kzg` | EIP-4844 blob data + KZG commitments (FFI) |
| `web3/erc4337` | Account abstraction (UserOp types) |
| `web3/batch-provider` | JSON-RPC batch requests |
| `web3/erc165` | Interface detection |
| `web3/revert` | Decode Solidity revert reasons |

## Architecture

```
                 +---------------------------+
                 |  web3/easy   (web3:)       |  CL one-liners + defcontract
                 +-------------+-------------+
                               |
                               v
                 +---------------------------+
                 |  web3/prelude              |  curated Coalton re-exports
                 +-------------+-------------+
                               |
                               v
              +-----------------------------------+
              |  Typed Coalton modules            |
              |  types -> rlp/crypto/address ->   |
              |  abi -> transaction -> provider   |
              |  -> wallet/contract/erc20/etc.    |
              +-----------------------------------+
```

## Examples

Self-contained programs in [`examples/`](examples/):

```bash
sbcl --load examples/01-balance-read.lisp        # ETH + USDC balance from mainnet
sbcl --load examples/02-defcontract-erc20.lisp   # defcontract over an inline ERC-20 ABI
anvil &                                          # for the next one
sbcl --load examples/03-send-eth.lisp            # sign + send + wait against Anvil
```

Inside this repository, `sbcl` means the project container:
`scripts/docker-sbcl.sh --non-interactive --load examples/01-balance-read.lisp`.

## Development

This is a Common Lisp Workbench (`cl-workbench`)
managed project: SBCL never runs on the host. Every build, eval, and test runs
in the pinned container image `web3-lisp-sbcl:2.5.2-1` (`docker/Dockerfile`),
which installs the Quicklisp 2025-06-22 dist and therefore the Coalton release
the code is written against. The full loop, eval exit codes, and restart rules
are in `.cl-workbench/WORKFLOW.md`; in short:

```bash
cl-workbench doctor --strict     # once per session, from the repo root
scripts/dev.sh start             # warm image (first start builds the image and compiles Coalton: minutes)
scripts/dev.sh eval '(+ 1 2)'    # ~0.3 s per eval
scripts/dev.sh test rlp          # one module: (web3-tests/runner::run-rlp-tests)
scripts/dev.sh test              # the whole suite in the warm image
scripts/dev.sh stop
```

`refs/` (git-ignored, see [CLAUDE.md](CLAUDE.md)) holds reference clones for
**reading only** — ethers.js, viem, and a newer Coalton master. The container's
ASDF registry excludes `refs/` on purpose: loading `refs/coalton` fails with a
package-lock error on `coalton-library/classes::optional/some`, because the
newer master renames the internals this library reaches into.

## Testing

```bash
# Hermetic suite (1000 tests, no network) in a fresh container — the verification of record
scripts/docker-test.sh
scripts/docker-test.sh rlp       # one module

# The same suite in the warm image, for iteration (see Development above)
scripts/dev.sh test
```

Both entry points run the same Lisp form, fail on any failing test or on a
selection that runs zero tests, and print `cl-workbench-checks: N` for the
counted gate.

The integration tests read `WEB3_INTEGRATION=1` and `WEB3_TEST_RPC_URL` from
the SBCL process's environment. Both wrappers forward those two variables into
the container whenever they are set in the calling shell (unset variables are
not forwarded, so the hermetic battery never sees an empty override):

```bash
# Cold battery against a live node or Anvil
WEB3_INTEGRATION=1 WEB3_TEST_RPC_URL=https://rpc.example \
  scripts/docker-test.sh

# Warm image: the container inherits the variables at START, so set them on
# the shell that starts it (or stop + start to change them)
WEB3_INTEGRATION=1 WEB3_TEST_RPC_URL=https://rpc.example scripts/dev.sh start
scripts/dev.sh identity passthrough-env   # lists the forwarded variables currently set
scripts/dev.sh test
```

The URL is resolved from inside the container: `127.0.0.1` there is the
container itself, so an Anvil must be reachable over Docker networking
(for example `http://host.docker.internal:8545`).

## Dependencies

- [Coalton](https://github.com/coalton-lang/coalton) — Statically typed language for Common Lisp
- [ironclad](https://github.com/sharplispers/ironclad) — Cryptographic primitives
- [dexador](https://github.com/fukamachi/dexador) — HTTP client
- [cl-json](https://github.com/sharplispers/cl-json) — JSON parsing
- [websocket-driver-client](https://github.com/fukamachi/websocket-driver) — WebSocket client
- [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) — Threading primitives
- [cffi](https://github.com/cffi/cffi) — C FFI (for KZG)
- [split-sequence](https://github.com/sharplispers/split-sequence) — String splitting

## Related projects

- [ssz-lisp](https://github.com/samdefmacro/ssz-lisp) — Ethereum SSZ serialization in Coalton

## License

MIT
