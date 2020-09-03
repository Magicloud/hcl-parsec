{-# LANGUAGE TemplateHaskell #-}
module Data.HCL.Types where

import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Void (Void)
import Optics.Lens
import Optics.Prism
import Optics.TH
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Pos (SourcePos)

type Parser = Parsec Void Text

data PosedText = PT { _from :: SourcePos
                    , _to :: SourcePos
                    , _text :: Text }
               deriving (Eq, Ord, Show)
makeFieldLabels ''PosedText
makeLenses ''PosedText

data StringPart = SPPlain PosedText
                | SPInterp PosedText
                deriving (Eq, Ord, Show)
makePrisms ''StringPart

type Doc = [Value]

data Value = VNumber SourcePos SourcePos Scientific
           | VString SourcePos SourcePos [StringPart]
           | VBool SourcePos SourcePos Bool
           | VIdents SourcePos SourcePos [PosedText]
           | VObject SourcePos SourcePos [PosedText] [Value] [Value] -- assignment object
           | VMap SourcePos SourcePos (Map PosedText Value)
           | VList SourcePos SourcePos [Value]
           | VFunction SourcePos SourcePos PosedText [Value]
           | VPair SourcePos SourcePos PosedText Value
           | VIndexing SourcePos SourcePos Value Value -- array index
           | VUnit
           deriving (Eq, Ord, Show)
makePrisms ''Value
