# hcl-parsec

This library aims to provide the abilities of processing HCL (Hashicorp Configuration Language) is Haskell. Existing `language-hcl` hackage does not seem to work any more.

Right now, the library supports parsing latest syntax. And people said Lens is a bad idea.

`examples` folder provides two samples of using the library. They should not be considered strict/safe or to use as serious tools.

The rendering (from parsing result to HCL code) is not planned.
