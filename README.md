# web3-lisp

Ethereum library implemented in [Coalton](https://github.com/coalton-lang/coalton), a statically typed language embedded in Common Lisp. Modeled after [ethers.js](https://github.com/ethers-io/ethers.js) and [viem](https://github.com/wevm/viem).

Type-safe interfaces for all major Ethereum operations: transactions, ABI encoding, contract interaction, token standards, HD wallets, ENS, WebSocket subscriptions, and more.

## Quickstart

Requires [SBCL](http://www.sbcl.org/) and [Quicklisp](https://www.quicklisp.org/).

```lisp
;; Load the full library
(asdf:load-system "web3")

;; Or load individual modules
(asdf:load-system "web3/provider")
(asdf:load-system "web3/wallet")
```

## Usage

### Connect to Ethereum

```lisp
(coalton-toplevel
  (define provider (make-http-provider "https://eth-mainnet.g.alchemy.com/v2/YOUR-KEY"))

  ;; Get latest block number
  (define block-num (eth-block-number provider))

  ;; Get balance
  (define balance
    (match (address-from-hex "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045")
      ((Ok addr) (eth-get-balance provider addr))
      ((Err e) (Err e)))))
```

### Wallets & Transactions

```lisp
(coalton-toplevel
  ;; Create wallet from private key
  (define my-wallet
    (wallet-with-provider
      (hex-decode-unsafe "0x...")  ; private key bytes
      (make-http-provider "https://...")))

  ;; Build and sign an EIP-1559 transaction
  (define tx
    (make-transaction
      EIP1559Tx
      1          ; chain-id (mainnet)
      0          ; nonce
      (u256-from-integer 2000000000)   ; max priority fee
      (u256-from-integer 30000000000)  ; max fee per gas
      21000      ; gas limit
      (Some to-address)
      (u256-from-integer 1000000000000000000) ; 1 ETH
      bytes-empty ; no calldata
      Nil))       ; no access list

  ;; Sign and send
  (define tx-hash (wallet-send-transaction my-wallet tx)))
```

### ERC-20 Tokens

```lisp
(coalton-toplevel
  ;; Read token info
  (define name   (erc20-name provider usdt-address))
  (define symbol (erc20-symbol provider usdt-address))
  (define bal    (erc20-balance-of provider usdt-address holder))

  ;; Build transfer calldata
  (define data (erc20-transfer-data recipient amount)))
```

### HD Wallets (BIP-39 / BIP-32)

```lisp
(coalton-toplevel
  ;; Generate a new 12-word mnemonic
  (define mnemonic (generate-mnemonic 128))

  ;; Derive Ethereum private key (m/44'/60'/0'/0/0)
  (define private-key (mnemonic-to-private-key mnemonic ""))

  ;; Or derive address directly
  (define addr (mnemonic-to-address mnemonic "")))
```

### ABI Encoding/Decoding

```lisp
(coalton-toplevel
  ;; Compute function selector
  (define sel (function-selector "transfer(address,uint256)"))

  ;; Encode arguments
  (define calldata
    (abi-encode-with-selector sel
      (Cons (AbiAddressVal addr-bytes)
            (Cons (AbiUintVal amount) Nil))))

  ;; Decode return data
  (define result
    (abi-decode (Cons AbiUint256 (Cons AbiBool Nil)) return-bytes)))
```

### ENS Resolution

```lisp
(coalton-toplevel
  ;; Compute namehash
  (define node (namehash "vitalik.eth"))

  ;; Resolve via provider
  (define addr (ens-resolve provider "vitalik.eth")))
```

### WebSocket Subscriptions

```lisp
;; Connect (Common Lisp level)
(let ((provider (web3/ws-provider:ws-connect "wss://eth-mainnet.g.alchemy.com/v2/YOUR-KEY")))
  ;; Subscribe to new blocks
  (web3/ws-provider:ws-subscribe provider
    (coalton:coalton web3/ws-provider:SubNewHeads)
    (lambda (json) (format t "New block: ~A~%" json)))

  ;; ... later
  (web3/ws-provider:ws-close provider))
```

### Unit Conversions

```lisp
(coalton-toplevel
  ;; Parse "1.5" ETH to wei (1500000000000000000)
  (define wei (parse-units "1.5" 18))

  ;; Format wei back to ETH string
  (define eth-str (format-units (u256-from-integer 1500000000000000000) 18)))
```

## Modules

33 independent ASDF subsystems, loadable individually or all at once via `"web3"`.

| Module | Description |
|--------|-------------|
| `web3/types` | Core types: `Bytes`, `U256`, hex encoding |
| `web3/rlp` | RLP encoding/decoding |
| `web3/crypto` | keccak256, secp256k1, ECDSA signatures |
| `web3/address` | Ethereum addresses with EIP-55 checksums |
| `web3/abi` | ABI encoding/decoding (uint, bool, address, bytes, string, arrays, tuples) |
| `web3/abi-parser` | Parse Solidity JSON ABI files |
| `web3/transaction` | Transaction types (legacy, EIP-2930, EIP-1559, EIP-4844), encoding, signing |
| `web3/provider` | JSON-RPC HTTP provider with 18+ methods |
| `web3/wallet` | Private key wallet with signing and sending |
| `web3/contract` | High-level contract abstraction from ABI JSON |
| `web3/erc20` | ERC-20 token standard (read + write calldata) |
| `web3/erc721` | ERC-721 NFT standard |
| `web3/erc1155` | ERC-1155 multi-token standard |
| `web3/events` | Event log parsing and decoding |
| `web3/logs` | Event log querying via `eth_getLogs` |
| `web3/ens` | ENS namehash (EIP-137) |
| `web3/ens-resolver` | Live ENS resolution via provider |
| `web3/deploy` | CREATE/CREATE2 address computation |
| `web3/multicall` | Multicall3 batching |
| `web3/eip712` | EIP-712 typed data hashing and signing |
| `web3/signature` | EIP-191 personal sign and recovery |
| `web3/siwe` | Sign-In with Ethereum (ERC-4361) |
| `web3/hdwallet` | BIP-39 mnemonics + BIP-32 key derivation |
| `web3/ws-provider` | WebSocket subscriptions (newHeads, logs, syncing, pendingTxs) |
| `web3/gas` | EIP-1559 fee calculation and estimation |
| `web3/simulate` | Transaction simulation and gas estimation |
| `web3/nonce-manager` | Multi-address/chain nonce tracking |
| `web3/receipt` | Transaction receipt parsing |
| `web3/block` | Block and header parsing |
| `web3/chain` | Pre-configured network settings |
| `web3/units` | `parseUnits`/`formatUnits` with custom decimals |
| `web3/blob` | EIP-4844 blob data encoding |
| `web3/kzg` | KZG commitments for blob transactions (via FFI) |

## Architecture

```
types (Bytes, U256, hex)
├── rlp (RLP encoding/decoding)
├── crypto (keccak256, secp256k1) [ironclad]
│   └── address (EIP-55 checksum)
│       ├── abi (ABI encode/decode)
│       │   ├── events (log parsing)
│       │   ├── deploy (CREATE/CREATE2)
│       │   ├── ens (namehash)
│       │   │   └── ens-resolver (live resolution via provider)
│       │   ├── multicall (Multicall3 batching)
│       │   ├── eip712 (typed data hashing)
│       │   └── abi-parser (Solidity JSON ABI)
│       │       └── contract (high-level abstraction)
│       ├── transaction (tx types, encode, sign)
│       │   └── provider (JSON-RPC) [dexador, cl-json]
│       │       ├── wallet (private key + provider)
│       │       ├── erc20, erc721, erc1155 (token standards)
│       │       ├── nonce-manager (nonce tracking)
│       │       ├── simulate (tx simulation)
│       │       └── logs (eth_getLogs)
│       └── signature (EIP-191 personal sign)
│           └── siwe (ERC-4361)
├── chain (network configs)
├── units (parseUnits/formatUnits)
├── gas (EIP-1559 fees) [cl-json]
├── block (block parsing) [cl-json]
├── receipt (receipt parsing) [cl-json]
├── ws-provider (WebSocket subscriptions) [websocket-driver, bordeaux-threads]
├── blob (EIP-4844 encoding) [ironclad]
│   └── kzg (KZG commitments) [cffi]
└── hdwallet (BIP-39/BIP-32) [ironclad]
```

## Testing

```bash
# Run all tests (837 tests)
sbcl --non-interactive \
  --eval '(asdf:load-system "web3/tests")' \
  --eval '(web3-tests/runner:run-all-tests)'

# Integration tests (requires Anvil or live node)
WEB3_INTEGRATION=1 WEB3_TEST_RPC_URL=http://127.0.0.1:8545 \
sbcl --non-interactive \
  --eval '(asdf:load-system "web3/tests")' \
  --eval '(web3-tests/runner:run-all-tests)'
```

## Dependencies

- [Coalton](https://github.com/coalton-lang/coalton) — Statically typed language for Common Lisp
- [ironclad](https://github.com/sharplispers/ironclad) — Cryptographic primitives
- [dexador](https://github.com/fukamachi/dexador) — HTTP client
- [cl-json](https://github.com/sharplispers/cl-json) — JSON parsing
- [websocket-driver-client](https://github.com/fukamachi/websocket-driver) — WebSocket client
- [bordeaux-threads](https://github.com/sionescu/bordeaux-threads) — Threading primitives
- [cffi](https://github.com/cffi/cffi) — C FFI (for KZG)
- [split-sequence](https://github.com/sharplispers/split-sequence) — String splitting

## Related Projects

- [ssz-lisp](https://github.com/samdefmacro/ssz-lisp) — Ethereum SSZ serialization in Coalton

## License

MIT
