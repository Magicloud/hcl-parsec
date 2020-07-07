{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.HCL
import Text.Megaparsec
import NeatInterpolation

test = [text|
{
  http_port = 80
  db_host   = module.rds.endpoint
  db_db     = "confluence"
  db_user   = "confluence"
  db_pass   = var.db_password
}
|]

main :: IO ()
main = print $ runParser (hclMap <* eof) "" test
