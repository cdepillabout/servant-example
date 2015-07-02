{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
    ( (:>), (:<|>)((:<|>)), Get, JSON, Proxy(..), ServantErr, ServerT, serve )

data Lala = Lala

type MyAPI = "dogs" :> Get '[JSON] [Lala]
        :<|> "cats" :> Get '[JSON] [String]

app :: Application
app = serve (Proxy :: Proxy MyAPI) server

server :: ServerT MyAPI (EitherT ServantErr IO)
server = dogNums :<|> cats

dogNums :: EitherT ServantErr IO [Lala]
dogNums = return [Lala]

cats :: EitherT ServantErr IO [String]
cats = return ["long-haired", "short-haired"]

main :: IO ()
main = run 32323 $ logStdoutDev app
