{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.HCL
import Data.HCL.Types
import Data.Text.IO as T

import System.Environment

import Text.Megaparsec

main :: IO ()
main =
  do home <- getEnv "HOME"
     T.readFile (home
                 ++ "/src/Work/terraform-aws-foundation/modules/vpc-scenario-2/variables.tf")
       >>= print
       . filter (not . varWithDesc)
       . fromRight []
       . runParser (hclDoc <* eof) "variables.tf"

varWithDesc :: Value -> Bool
varWithDesc (VObject _ _ (PT _ _ "variable":_) ps _) =
  case filter (\case
                 VPair _ _ (PT _ _ "description") _ -> True
                 _ -> False)
              ps of
    [] -> False
    _ -> True
varWithDesc _ = False
