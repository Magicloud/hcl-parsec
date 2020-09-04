{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.HCL where

import Control.Monad

import Data.Char
import Data.HCL.Types
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import Data.Text ( Text )

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

-- import           Text.Megaparsec.Debug
hclIdent :: Parser PosedText
hclIdent =
  label "hclIdent"
  $ do s <- getSourcePos
       i <- takeWhile1P Nothing (\c -> isAlphaNum c || c `elem` [ '_', '-' ])
       e <- getSourcePos
       return $ PT s e i

hclIdents :: Parser Value
hclIdents =
  label "hclIdents"
  $ do s <- getSourcePos
       is <- hclIdent `sepBy1` char '.'
       e <- getSourcePos
       return $ VIdents s e is

hclNumber :: Parser Value
hclNumber =
  label "hclNumber"
  $ do s <- getSourcePos
       n <- scientific
       e <- getSourcePos
       return $ VNumber s e n

hclBool :: Parser Value
hclBool =
  label "hclBool"
  $ do s <- getSourcePos
       b <- strTrue <|> strFalse
       e <- getSourcePos
       return $ VBool s e b
  where strTrue =
          hclSymbol "true" <|> hclSymbol "TRUE" <|> hclSymbol "True"
          >> return True

        strFalse =
          hclSymbol "false" <|> hclSymbol "FALSE" <|> hclSymbol "False"
          >> return False

hclString :: Parser Value
hclString =
  label "hclString"
  $ do s <- getSourcePos
       void $ char '"'
       str <- manyTill hclStringPart $ char '"'
       e <- getSourcePos
       return $ VString s e str

hclStringPart :: Parser StringPart
hclStringPart =
  try hclStringInterpEscape <|> hclStringInterp <|> hclStringPlain <|> eol'
  where eol' :: Parser StringPart
        eol' =
          do s <- getSourcePos
             r <- eol
             e <- getSourcePos
             return $ SPPlain $ PT s e r

hclStringInterpEscape :: Parser StringPart
hclStringInterpEscape =
  label "hclStringInterpEscape"
  $ do s <- getSourcePos
       void $ string "$${"
       e <- getSourcePos
       return $ SPPlain $ PT s e "${"

hclStringInterp :: Parser StringPart
hclStringInterp =
  label "hclStringInterp"
  $ do s <- getSourcePos
       void $ string "${"
       str <- takeWhile1P Nothing (/= '}')
       void $ char '}'
       e <- getSourcePos
       return $ SPInterp $ PT s e str

hclStringPlain :: Parser StringPart
hclStringPlain =
  label "hclStringPlain"
  $ do let end =
             eof
             <|> void eol
             <|> lookAhead (void $ char '"')
             <|> lookAhead (void $ string "${")
             <|> lookAhead (void $ string "$${")
       s <- getSourcePos
       str <- manyTill charLiteral end
       e <- getSourcePos
       return $ SPPlain $ PT s e $ Text.pack str

hclStringML :: Parser Value
hclStringML =
  label "hclStringPlainML"
  $ do s <- getSourcePos
       void $ string "<<"
       indent <- optional $ char '-'
       eof' <-
         takeWhile1P Nothing (\c -> isAlphaNum c || c `elem` [ '_', '-', '.' ])
       void eol
       sps <- manyTill hclStringPart
                       (when (isJust indent) hclSkipSpace' >> string eof')
       e <- getSourcePos
       return $ VString s e sps

hclLexeme :: Parser a -> Parser a
hclLexeme = L.lexeme hclSkipSpace'

hclSymbol :: Text -> Parser Text
hclSymbol = L.symbol hclSkipSpace'

hclSkipLineComment :: Parser ()
hclSkipLineComment =
  label "hclSkipLineComment" $ skipLineComment "#" <|> skipLineComment "//"

hclSkipBlockComment :: Parser ()
hclSkipBlockComment = label "hclSkipBlockComment" $ skipBlockComment "/*" "*/"

hclSkipSpace :: Parser Value
hclSkipSpace = hclSkipSpace' >> return VUnit

hclSkipSpace' :: Parser ()
hclSkipSpace' = L.space space1 hclSkipLineComment hclSkipBlockComment

hclIndexing :: Parser Value
hclIndexing =
  label "hclIndexing"
  $ do s <- getSourcePos
       array <-
         hclLexeme $ try hclFunction <|> hclIdents <|> hclList <|> hclMap
       void $ hclSymbol "["
       index <- hclLexeme hclValue
       void $ hclSymbol "]"
       e <- getSourcePos
       return $ VIndexing s e array index

hclList :: Parser Value
hclList =
  label "hclList"
  $ do s <- getSourcePos
       void $ hclSymbol "["
       vs <- hclLexeme $ sepByComma hclValue
       void $ optional $ hclSymbol ","
       void $ hclSymbol "]"
       e <- getSourcePos
       return $ VList s e vs

hclValue :: Parser Value
hclValue =
  label "hclValue"
  $ choice [ hclNumber
           , try
               $ choice [ try hclFunction, hclIdents, hclList, hclMap ]
               <* notFollowedBy (char '[')
           , hclIndexing
           , hclString
           , hclBool
           , hclObject ]

hclFunction :: Parser Value
hclFunction =
  label "hclFunction"
  $ do s <- getSourcePos
       fn <- hclIdent
       void $ hclSymbol "("
       as <- hclLexeme $ sepByComma hclValue
       void $ hclSymbol ")"
       e <- getSourcePos
       return $ VFunction s e fn as

hclMap :: Parser Value
hclMap =
  label "hclMap"
  $ do s <- getSourcePos
       void $ hclSymbol "{"
       ps <- hclLexeme $ hclPair `sepEndBy` (hclSymbol "\n" <|> hclSymbol ",")
       void $ hclSymbol "}"
       e <- getSourcePos
       return $ VMap s e $ Map.fromList $ map (\(VPair _ _ k v) -> ( k, v )) ps

sepByComma :: Parser a -> Parser [ a ]
sepByComma x = x `sepBy` hclSymbol ","

-- no object
hclPair :: Parser Value
hclPair =
  label "hclPair"
  $ do s <- getSourcePos
       k <- hclLexeme_ $ doubleQuotedString <|> hclIdent
       void $ hclSymbol "="
       v <- hclLexeme_ $ hclStringML <|> hclValue
       e <- getSourcePos
       return $ VPair s e k v
  where hclLexeme_ = L.lexeme hclSkipSpace_

        hclSkipSpace_ = L.space space1_ hclSkipLineComment hclSkipBlockComment

        space1_ = void $ takeWhile1P (Just "white space") notNewlineButSpace

        notNewlineButSpace c = c /= '\n' && isSpace c

doubleQuotedString :: Parser PosedText
doubleQuotedString =
  label "doubleQuotedString"
  $ do s <- getSourcePos
       void $ char '"'
       t <- takeWhileP Nothing (/= '"')
       void $ char '"'
       e <- getSourcePos
       return $ PT s e t

hclObject :: Parser Value
hclObject =
  label "hclObject"
  $ do s <- getSourcePos
       is <- hclLexeme
         $ manyTill (hclLexeme $ doubleQuotedString <|> hclIdent)
         $ char '{'
       vs <- hclLexeme
         $ manyTill (hclLexeme $ try hclPair <|> hclObject)
         $ char '}'
       let ( os_, as_ ) =
             foldr (\i ( os, as ) -> case i of
                      o@VObject {} -> ( o:os, as )
                      a@VPair {} -> ( os, a:as )
                      _ -> ( os, as ))
                   ( [], [] )
                   vs
       e <- getSourcePos
       return $ VObject s e is as_ os_

hclDoc :: Parser Doc
hclDoc =
  many
  $ do void hclSkipSpace
       hclObject
