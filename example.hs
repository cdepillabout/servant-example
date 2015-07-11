{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
    ( (:>), (:<|>)((:<|>)), Get, JSON, Proxy(..), ServantErr, ServerT, serve )

-- | A representation of our REST API at the type level.
--
-- This defines two routes:
--   * /dogs -- Responds to HTTP GET with a list of integers in JSON format.
--   * /cats -- Responds to HTTP GET with a list of Strings in JSON format.
type MyAPI = "dogs" :> Get '[JSON] [Int]
        :<|> "cats" :> Get '[JSON] [String]

-- | A Warp 'Application' that will serve our API.
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

-- | Our entire API.  You can see that it is a combination of the 'dogNums'
-- handler and the 'cats' handler.

-- One way of writing the type.
-- myAPI :: EitherT ServantErr IO [Int]
--     :<|> EitherT ServantErr IO [String]
--
-- Another way of writing the type.
-- myAPI :: ServerT (Get '[JSON] [Int]) (EitherT ServantErr IO)
--     :<|> ServerT (Get '[JSON] [String]) (EitherT ServantErr IO)
--
-- The shortest way of writing the type.
myAPI :: ServerT MyAPI (EitherT ServantErr IO)
myAPI = dogNums :<|> cats

-- | A handler for the @/dogs@ route.  It just returns a list of the integers
-- one to four.
dogNums :: EitherT ServantErr IO [Int]
dogNums = return [1,2,3,4]

-- | A handler for the @/cats@ route.
cats :: EitherT ServantErr IO [String]
cats = return ["long-haired", "short-haired"]

-- | Run our 'app' as a Warp 'Application'.
main :: IO ()
main = run 32323 $ logStdoutDev app

