# Examples

Self-contained programs you can run with `sbcl --load examples/<file>.lisp`.

| File | What it does | Network? |
|------|--------------|----------|
| [`01-balance-read.lisp`](01-balance-read.lisp) | Reads ETH and USDC balance for a hard-coded address. | Public RPC (mainnet) |
| [`02-defcontract-erc20.lisp`](02-defcontract-erc20.lisp) | `defcontract` against an inline ERC-20 ABI. Builds calldata offline; live reads when `WEB3_INTEGRATION=1`. | Optional |
| [`03-send-eth.lisp`](03-send-eth.lisp) | Signs + broadcasts an ETH transfer, then waits for the receipt. | Local Anvil |

## Setup

```bash
# Library (and Quicklisp / SBCL) must be available.
asdf:load-system "web3"  is invoked by each example.

# For example 03 you need a local node:
anvil &                                                    # one terminal
sbcl --load examples/03-send-eth.lisp                      # another
```

## Environment variables honored

| Variable | Default | Used by |
|----------|---------|---------|
| `WEB3_RPC_URL` | `https://eth.llamarpc.com` (or `http://127.0.0.1:8545` for example 03) | All examples |
| `ADDRESS` | vitalik.eth's address | example 01 |
| `WEB3_INTEGRATION` | unset | example 02 (gates the live read) |
