{-# LANGUAGE OverloadedStrings #-}
module DevelMain where

import Data.ByteString.Lazy (ByteString)
import Data.Map
import System.Random

import Thracery

testData :: ByteString
testData = "{\"substance\":[\"mist\", \"fog\"],\"adj\":[\"fair\", \"bright\", \"splendid\"],\"origin\":[\"#adj# #substance#\"]}"

testRawGrammar :: RawGrammar
testRawGrammar = fromList [("substance", ["mist", "fog"]), ("adj", ["fair", "bright"]), ("origin", ["#adj# #substance#"])]

testGrammar :: Grammar
testGrammar = fromList [("substance", [[Raw "mist"], [Raw "fog"]]), ("adj", [[Raw "fair"], [Raw "bright"]]), ("origin", [[SymbolRef "adj" [], SymbolRef "substance" []]])]

testRule :: Rule
testRule = [Raw "I Love my ", SymbolRef "adj" [], Raw " ", SymbolRef "substance" [], Raw " ", Loop [SymbolRef "substance" []]]

testNode :: TNode
testNode = TNode (mkStdGen 123) testGrammar testRule "" Nothing

