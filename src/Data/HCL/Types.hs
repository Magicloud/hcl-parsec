module Data.HCL.Types where

import Data.Map ( Map )
import Data.Scientific ( Scientific )
import Data.Text ( Text )
import Data.Void ( Void )

import Text.Megaparsec ( Parsec )
import Text.Megaparsec.Pos ( SourcePos )

type Parser = Parsec Void Text

type Doc = [ Value ]

data PosedText =
  PT { ptFrom :: SourcePos
     , ptTo :: SourcePos
     , ptText :: Text }
  deriving ( Eq, Ord, Show )

data Value
  = VNumber SourcePos SourcePos Scientific
  | VString SourcePos SourcePos [ StringPart ]
  | VBool SourcePos SourcePos Bool
  | VIdents SourcePos SourcePos [ PosedText ]
  | VObject SourcePos SourcePos [ PosedText ] [ Value ] [ Value ] -- assignment object
  | VMap SourcePos SourcePos (Map PosedText Value)
  | VList SourcePos SourcePos [ Value ]
  | VFunction SourcePos SourcePos PosedText [ Value ]
  | VPair SourcePos SourcePos PosedText Value
  | VIndexing SourcePos SourcePos Value Value -- array index
  | VUnit
  deriving ( Eq, Ord, Show )

data StringPart
  = SPPlain PosedText
  | SPInterp PosedText
  deriving ( Eq, Ord, Show )
