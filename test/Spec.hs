{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Data.Aeson
import Unison.MCP.Protocol
import Unison.MCP.Tools
import qualified IntegrationSpec

main :: IO ()
main = hspec $ do
  IntegrationSpec.spec
  
  describe "Protocol" $ do
    it "encodes and decodes requests" $ do
      let req = Request
            { requestJsonrpc = "2.0"
            , requestId = IdInt 1
            , requestMethod = Initialize
            , requestParams = Nothing
            }
      (eitherDecode . encode) req `shouldBe` Right req
    
    it "encodes responses correctly" $ do
      let resp = Response
            { responseJsonrpc = "2.0"
            , responseId = Just (IdString "test")
            , responseResult = Just (String "result")
            , responseError = Nothing
            }
      -- Just check that encoding works
      encode resp `shouldNotBe` ""
  
  describe "Tools" $ do
    it "provides the correct number of tools" $ do
      length availableTools `shouldBe` 25
    
    it "all tools have valid schemas" $ do
      mapM_ checkToolSchema availableTools

checkToolSchema :: Tool -> Expectation
checkToolSchema tool = do
  toolName tool `shouldNotBe` ""
  case toolInputSchema tool of
    Object o -> o `shouldNotBe` mempty
    _ -> expectationFailure "Tool schema should be an object"