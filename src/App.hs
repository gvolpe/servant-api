{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module App ( startServer, test ) where

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- | A greet message data type
newtype Greet = Greet { msg :: Text } deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type Api =
       -- GET / (root) returns a welcome message as plain text
       Get '[PlainText] Text      
 
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  :<|> "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

api :: Proxy Api
api = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server Api
server = homeH :<|> helloH :<|> postGreetH :<|> deleteGreetH

  where homeH = return "Servant web server running on Haskell!" 

        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH greet = return greet

        deleteGreetH _ = return NoContent

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve api server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
startServer :: IO ()
startServer = run 8080 test
