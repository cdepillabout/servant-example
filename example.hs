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

type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

myAPI :: ServerT ("dogs" :> Get '[JSON] [Int]) (EitherT ServantErr IO)
    :<|> ServerT ("cats" :> Get '[JSON] [String]) (EitherT ServantErr IO)
-- myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

cats :: EitherT ServantErr IO [String]
cats = return ["long-haired", "short-haired"]

main :: IO ()
main = run 32323 $ logStdoutDev app

