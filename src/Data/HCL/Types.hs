{-# LANGUAGE TemplateHaskell #-}

module Data.HCL.Types where

import Data.Map ( Map )
import Data.Scientific ( Scientific )
import Data.Text ( Text )
import Data.Void ( Void )

import Optics.Lens
import Optics.Prism
import Optics.TH

import Text.Megaparsec ( Parsec )
import Text.Megaparsec.Pos ( SourcePos )

type Parser = Parsec Void Text

data PosedText =
  PT { _ptFrom :: SourcePos
     , _ptTo :: SourcePos
     , _text :: Text }
  deriving ( Eq, Ord, Show )

makeFieldLabels ''PosedText

makeLenses ''PosedText

data StringPart
  = SPPlain PosedText
  | SPInterp PosedText
  deriving ( Eq, Ord, Show )

makePrisms ''StringPart

type Doc = [ Value ]

data Value
  = VNumber { _vnFrom :: SourcePos
            , _vnTo :: SourcePos
            , _vn :: Scientific }
  | VString { _vsFrom :: SourcePos
            , _vsTo :: SourcePos
            , _vsString :: [ StringPart ] }
  | VBool { _vbFrom :: SourcePos
          , _vbTo :: SourcePos
          , _vbBool :: Bool }
  | VIdents { _viFrom :: SourcePos
            , _viTo :: SourcePos
            , _vi :: [ PosedText ] }
  | VObject { _voFrom :: SourcePos
            , _voTo :: SourcePos
            , _voIdents :: [ PosedText ]
            , _voAssignments :: [ Value ]
            , _voNestedObjects :: [ Value ] }
  | VMap { _vmFrom :: SourcePos
         , _vmTo :: SourcePos
         , _vmMap :: Map PosedText Value }
  | VList { _vlFrom :: SourcePos
          , _vlTo :: SourcePos
          , _vlList :: [ Value ] }
  | VFunction { _vfFrom :: SourcePos
              , _vfTo :: SourcePos
              , _vfName :: PosedText
              , _vfParameters :: [ Value ] }
  | VPair { _vpFrom :: SourcePos
          , _vpTo :: SourcePos
          , _vpKey :: PosedText
          , _vpValue :: Value }
  | VIndexing { _viFrom :: SourcePos
              , _viTo :: SourcePos
              , _viList :: Value
              , _viIndex :: Value }
  | VUnit
  deriving ( Eq, Ord, Show )

makePrisms ''Value

makeLenses ''Value
