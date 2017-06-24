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
data Val = Val { valMessage :: Message, valSubscription :: Text } deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance ToJSON Val where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Val where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

