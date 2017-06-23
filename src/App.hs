{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import qualified Option as O
import Option (Option)
import     Control.Monad
import Data.Either.Combinators
import           Control.Monad.Trans.Class    (lift)
import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import Servant.Client
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import Network.HTTP.Client hiding (Proxy)

data Val = Val { name :: String, value :: Int } deriving (Show, Generic, FromJSON, ToJSON)

type MyApi = "list" :> Get '[JSON] (Maybe Val)

myApi :: Proxy MyApi
myApi = Proxy

getAllBooks :: ClientM (Maybe Val)
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

run :: Option -> IO ()
run o = do
  let settings =
        setPort (O.port o) $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ (show $ O.port o))) defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

type ItemApi = "item" :> Get '[JSON] (Maybe Val)

api :: Proxy ItemApi
api = Proxy

server :: Server ItemApi
server = lift app3

appMain2 :: IO (Either ServantErr (Maybe Val))
appMain2 = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapBoth (const err500) id <$> runClientM getAllBooks (ClientEnv manager (BaseUrl Http "google.com" 80 ""))

app3 :: IO (Maybe Val)
app3 = join . rightToMaybe <$> appMain2

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

