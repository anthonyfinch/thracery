{-# LANGUAGE OverloadedStrings #-}
module Thracery where

import System.Random

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import Data.Aeson

import Text.Megaparsec
import Text.Megaparsec.Text


type Symbol = T.Text
type Modifier = T.Text

data ActionVerb = APop
                | APush T.Text deriving Show

data RulePart = Raw T.Text
              | SymbolRef Symbol [Modifier]
              | Loop [RulePart]
              | Action Symbol ActionVerb deriving Show

type Rule = [RulePart]

type RawGrammar = M.Map T.Text [T.Text]
type Grammar = M.Map Symbol [Rule]

data TNode = TNode StdGen Grammar Rule T.Text (Maybe TNode)
           | TNodeT T.Text deriving Show

--
-- JSON -> RawGrammar decoding
--

decodeRawGrammar :: B.ByteString -> Maybe RawGrammar
decodeRawGrammar inp = decode inp

decodeGrammar :: B.ByteString -> Maybe Grammar
decodeGrammar inp =
  case rawGrammar of Nothing -> Nothing
                     Just raw -> Just $ parseGrammar raw
  where
    rawGrammar = decodeRawGrammar inp

parseGrammar :: RawGrammar -> Grammar
parseGrammar raw = foldl extractRules M.empty (M.toList raw)
  where
    extractRules grammar (k, rawRules) = M.insert k (parseRules rawRules)  grammar

readFileGrammar :: FilePath -> IO (Maybe Grammar)
readFileGrammar fp = do
  d <- B.readFile fp
  return $ decodeGrammar d


--
-- Rules Parsing
--

pRaw :: Parser RulePart
pRaw = do
  r <- some $ satisfy (/= '#')
  return $ Raw $ T.pack r

pSymbol :: Parser RulePart
pSymbol = do
  char '#'
  s <- some alphaNumChar <?> "symbol"
  ms <- many pModifier
  char '#'
  return $ SymbolRef (T.pack s) ms

pModifier :: Parser T.Text
pModifier = do
  char '.'
  m <- some alphaNumChar <?> "modifier"
  return $ T.pack m

pRule :: Parser [RulePart]
pRule = many (pSymbol <|> pRaw)

parseRule :: T.Text -> [RulePart]
parseRule raw = maybe [] id (parseMaybe pRule raw)

parseRules :: [T.Text] -> [Rule]
parseRules rawR = map parseRule rawR


--
-- TNode Generation
--

pickRule :: (StdGen, [Rule]) -> (StdGen, Rule)
pickRule (g, rs) =
  case length rs of 0 -> (g, [Raw ""])
                    l -> (g', rs !! ix)
                      where
                        (ix, g') = randomR (0, l-1) g

step :: TNode -> TNode
step (TNode _ _ [] txt Nothing) = TNodeT txt

step (TNode g gr rs txt (Just (TNodeT t))) = TNode g gr (Raw t:rs) txt Nothing -- NEEDS WORK
step (TNode g gr rs txt (Just child)) = TNode g gr rs txt $ Just $ step child

step (TNode g gr (Raw s:rs) txt n) = TNode g gr rs (txt `T.append` s) n
step (TNode g gr (Loop srs:rs) txt n) = TNode g gr rs txt $ Just (TNode g gr srs "" Nothing)
step (TNode g gr (Action s v:rs) txt n) = TNode g gr rs txt n -- NEEDS WORK
step (TNode g gr (SymbolRef s []:rs) txt n) = TNode g' gr (newRule ++ rs) txt n
  where
    rules = M.findWithDefault [] s gr
    (g', newRule) = pickRule (g, rules)


expand :: TNode -> (T.Text, [TNode])
expand node = loop [] node
  where
    loop ns n@(TNodeT t) = (t, ns ++ [n])
    loop ns n = loop (ns ++ [n]) (step n)

expand' :: TNode -> T.Text
expand' (TNodeT t) = t
expand' n = expand' $ step n


--
-- IO Running functions
--

getNode :: T.Text -> Grammar -> IO TNode
getNode txt gr = do
  gen <- newStdGen
  return $ TNode gen gr rule "" Nothing
  where
    rule = parseRule txt

run :: T.Text -> Grammar -> IO T.Text
run txt gr = do
  node <- getNode txt gr
  return $ expand' node
