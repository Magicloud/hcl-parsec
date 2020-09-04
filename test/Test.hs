{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.HCL

import NeatInterpolation

import Text.Megaparsec
import Text.Pretty.Simple ( pPrint )

test =
  [text|
module "test" {
  test = <<EOF
a$${b}c
de$${f}
$${g}hi
j$$$${k}l
mno
EOF
}
|]

main :: IO ()
main =
  pPrint
  $ runParser (hclDoc <* eof)
              "mock"
              "module \"test\" {\n  test = <<EOF\nj$${k}l\nEOF\n}"
