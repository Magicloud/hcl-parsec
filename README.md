# hcl-parsec

This library aims to provide the abilities of processing HCL (Hashicorp Configuration Language) is Haskell. Existing `language-hcl` hackage does not seem to work any more.

Right now, the library supports parsing latest syntax. And I am still wondering if Lens can be introduced to ease the accessing to the content within the parsing result.

`examples` folder provides two samples of using the library. They should not be considered strict/safe or to use as serious tools.

The rendering (from parsing result to HCL code) is not planned.
