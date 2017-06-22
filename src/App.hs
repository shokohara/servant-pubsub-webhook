{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import           Control.Monad.Trans.Class    (lift)
import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Either
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import Servant.Client
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import Network.HTTP.Client hiding (Proxy)
import Data.Either.Combinators

data Val = Val { name :: String, value :: Int } deriving (Show, Generic, FromJSON, ToJSON)

type MyApi = "list" :> Get '[JSON] [Val] -- GET /books

myApi :: Proxy MyApi
myApi = Proxy

getAllBooks :: ClientM [Val]
getAllBooks = client myApi

appMain :: IO ()
appMain = do
  manager <- newManager defaultManagerSettings
  res <- runClientM getAllBooks (ClientEnv manager (BaseUrl Http "google.com" 80 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right books -> print books

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

type ItemApi = "item" :> Get '[JSON] [Val]

api :: Proxy ItemApi
api = Proxy

server :: Server ItemApi
server = lift app3
--appMain2 :: EitherT ServantError IO [Val]
--appMain2 = do
--  manager <- liftIO $ newManager defaultManagerSettings
--  EitherT $ runClientM getAllBooks (ClientEnv manager (BaseUrl Http "google.com" 80 ""))
appMain2 :: IO (Either ServantErr [Val])
appMain2 = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapBoth (const err500) id <$> runClientM getAllBooks (ClientEnv manager (BaseUrl Http "google.com" 80 ""))

app3 :: IO [Val]
app3 = appMain2 >>= \x -> case x of
  Right x -> return x
  _ -> return $ fail ""

--getItems :: Handler [Val]
--getItems = lift appMain2

getItemById :: Integer -> Handler Item
getItemById = \ case
  0 -> return exampleItem
  _ -> throwE err404

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool

