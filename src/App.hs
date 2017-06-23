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
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import Servant.Client
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import Network.HTTP.Client hiding (Proxy)

data Val = Val { name :: String, value :: Int } deriving (Show, Generic, FromJSON, ToJSON)

type ClientApi = "api" :> Get '[JSON] (Maybe Val)

type ServerApi = "api" :> Get '[JSON] (Maybe Val)
--type ServerApi = "api" :> QueryParam "ip" String :> Get '[JSON] (Maybe Val)

clientApi :: Proxy ClientApi
clientApi = Proxy

serverApi :: Proxy ServerApi
serverApi = Proxy

getAllBooks :: ClientM (Maybe Val)
getAllBooks = client clientApi

appMain2 :: IO (Either ServantErr (Maybe Val))
appMain2 = do
  manager <- liftIO $ newManager defaultManagerSettings
  mapBoth (const err500) id <$> runClientM getAllBooks (ClientEnv manager (BaseUrl Http "google.com" 80 ""))

app3 :: IO (Maybe Val)
app3 = join . rightToMaybe <$> appMain2

server :: Server ServerApi
server = lift app3

mkApp :: IO Application
mkApp = return $ serve serverApi server

run :: Option -> IO ()
run o = do
  let settings =
        setPort (O.port o) $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ (show $ O.port o))) defaultSettings
  runSettings settings =<< mkApp


