{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Servant.PubSub.Webhook where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Casing

data Message = Message { messageAttributes :: Object, messageData :: Text, messageMessageId :: Text, messagePublishTime :: Text } deriving (Show, Eq, Generic)
data PubSubRequest = PubSubRequest { psrMessage :: Message, psrSubscription :: Text } deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance ToJSON PubSubRequest where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PubSubRequest where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

