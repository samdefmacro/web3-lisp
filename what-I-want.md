I want to implement a common lisp (with coalton language support) interface for Ethereum, just like below repos:

- https://github.com/ethers-io/ethers.js (downloaded to current project as a submodule)
- https://github.com/wevm/viem (downloaded to current project as a submodule)

The coalton language is list below for reference:

- https://github.com/coalton-lang/coalton (downloaded to current project as a submodule)


The implementation should have detailed test cases to cover the code. The test cases can be refered to ether.js or view.


It might need to construct many ethereum objects and may need many utility or helpers libraries, maintaining their relative independence and clear interfaces facilitates reuse in other projects in the future.


When need a ethereum ssz implementation in coalton, You can refer to the one that I have make before:

- ~/common-lisp/ssz-lisp-claude (Ethereum SSZ serialization in Coalton with full spec compliance)
