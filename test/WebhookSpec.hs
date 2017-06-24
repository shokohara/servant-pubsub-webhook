{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module WebhookSpec where

import Test.Hspec
import Data.String.Here
import Servant.PubSub.Webhook
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Lazy as H

spec :: Spec
spec = do
  describe "json" $ do
    it "request.json" $ do
      (decode $ C.pack [hereFile|request.json|]) `shouldBe` Just PubSubRequest { psrMessage = Message {messageAttributes = H.fromList [("string-value",String "string-value")], messageData = "base64-no-line-feeds-variant-representation-of-payload", messageMessageId = "string-value", messagePublishTime = "string-value"}, psrSubscription = "string-value"}

