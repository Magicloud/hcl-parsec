{-# LANGUAGE OverloadedStrings #-}
import           Data.Either
import           Data.HCL
import           Data.HCL.Types
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.IO as T
import           System.Environment
import           Text.Megaparsec
import Debug.Trace

data ModRef = MR { source :: PosedText
                 , version :: Maybe PosedText }

main :: IO ()
main = do
  args <- getArgs
  let file = head args
      mode = args !! 1
      base = T.pack $ args !! 2
      ver_ = if length args > 3
        then Just $ T.pack $ args !! 3
        else Nothing

  tf <- T.readFile file
  let parsed = runParser (hclDoc <* eof) file tf
      modSrcVers = mapMaybe modSV $ fromRight [] parsed
      toWrite = T.unlines $ case mode of
        "reg" -> -- Switch to Registry style
          foldl (\tfByLine (MR (PT s e t) _) ->
            let x = replaceAtWith tfByLine s e $ src2reg t base
            in maybe x (addVer x e) ver_) (T.lines tf) modSrcVers
        "src" -> -- Switch to Source style
          foldl (\tfByLine (MR (PT s e t) v) ->
            let tfByLine' = case v of
                  Nothing -> tfByLine
                  Just (PT sv ev _) ->
                    let (pre, _, post) = extractRange tfByLine (unPos $ sourceLine sv) (unPos $ sourceLine ev)
                    in pre ++ post
            in replaceAtWith tfByLine' s e $ reg2src t base ver_
            ) (T.lines tf) modSrcVers
        _ -> undefined
  T.writeFile file toWrite

addVer :: [Text] -> SourcePos -> Text -> [Text]
addVer c l v =
  let (pre, post) = splitAt (unPos $ sourceLine l) c
  in pre ++ [T.intercalate "" ["  version = \"", v, "\""]] ++ post

modSV :: Value -> Maybe ModRef
modSV (VObject _ _ (PT _ _ "module" : _) ps _) = Just $ foldl (\mr (VPair _ _ (PT _ _ k) v) ->
  case k of
    "source" ->
      let (VString _ _ [SPPlain pt]) = v
      in mr { source = pt }
    "version" ->
      let (VString _ _ [SPPlain pt]) = v
      in mr { version = Just pt }
    _ -> mr) (MR (PT (initialPos "") (initialPos "") "") Nothing) ps
modSV _ = Nothing

src2reg :: Text -> Text -> Text
src2reg src baseUrl =
  let parts = T.split (== '/') src
      (_, repo : modPath) = break ("terraform-" `T.isPrefixOf`) parts
      repoParts = T.split (== '-') repo
      platform = repoParts !! 1
      orgName = dropSuffix ".git" $ T.intercalate "-" $ drop 2 repoParts
  in T.intercalate "/" ([ baseUrl
                        , orgName
                        , platform ] ++ init modPath ++ [T.takeWhile (/= '?') (last modPath)])

reg2src :: Text -> Text -> Maybe Text -> Text
reg2src reg baseUrl ref =
  let parts = T.split (== '/') reg
      idxOfMod = fromJust $ elemIndex "modules" parts
      orgName = parts !! (idxOfMod - 3)
      platform = parts !! (idxOfMod - 2)
  in T.intercalate "/" ([ baseUrl
                        , T.intercalate "-" ["terraform", platform, orgName] `T.append` ".git" ] ++ drop (idxOfMod - 1) parts) `T.append` maybe "" ("?ref=" `T.append`) ref

dropSuffix :: Text -> Text -> Text
dropSuffix suf txt =
  fromMaybe txt $ T.stripSuffix suf txt

replaceAtWith :: [Text] -> SourcePos -> SourcePos -> Text -> [Text]
replaceAtWith c f t r =
  let fromLine = unPos $ sourceLine f
      fromCol = unPos $ sourceColumn f
      toLine = unPos $ sourceLine t
      toCol = unPos (sourceColumn t) - 1
      (pre, edit, post) = extractRange c fromLine toLine
      pre' = T.take (fromCol - 1) $ head edit
      post' = T.drop toCol $ last edit
      edit' = T.lines $ T.intercalate "" [pre', r, post']
  in pre ++ edit' ++ post

extractRange :: [a] -> Int -> Int -> ([a], [a], [a])
extractRange xs from to =
  let (ab, c) = splitAt to xs
      (a, b) = splitAt (from - 1) ab
  in (a, b, c)
