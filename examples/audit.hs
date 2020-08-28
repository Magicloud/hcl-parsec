{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.HCL
import Data.HCL.Types
import Data.Maybe
import Data.Text.IO as T

import Optics.Core

import System.Environment

import Text.Megaparsec
import Text.Pretty.Simple ( pPrint )

main :: IO ()
main =
  do home <- getEnv "HOME"
     T.readFile (home
                 ++ "/src/Work/terraform-aws-foundation/modules/vpc-scenario-2/variables.tf")
       >>= pPrint
       . filter (not . varWithDesc)
       . fromRight []
       . runParser (hclDoc <* eof) "variables.tf"

varWithDesc :: Value -> Bool
varWithDesc v =
  isJust (preview _VObject v)
  && preview (voIdents % ix 0 % text) v == Just "variable"
  && anyOf (voAssignments % each)
           ((==) (Just "description") . preview (vpKey % text))
           v
