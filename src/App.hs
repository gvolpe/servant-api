{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module App ( startServer ) where

import Control.Lens
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Swagger
import Data.Text
import Data.Typeable (Typeable)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Swagger

-- | A greet message data type
newtype Greet = Greet { msg :: Text } deriving (Generic, Show, Typeable)

instance FromJSON Greet
instance ToJSON Greet

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

api :: Proxy Api
api = Proxy

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

type API = SwaggerAPI :<|> Api

apiSwagger :: Swagger
apiSwagger = toSwagger api
  & info.title   .~ "Servant Demo API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

instance ToSchema Greet where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some Greet"
    & mapped.schema.example ?~ toJSON (Greet "Hello world!")

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server API
server = return apiSwagger :<|> homeH :<|> helloH :<|> postGreetH :<|> deleteGreetH

  where homeH = return "Servant web server running on Haskell!"

        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH = return

        deleteGreetH _ = return NoContent

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
--test :: Application
--test = serve api server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
startServer :: IO ()
startServer = do
  putStrLn "Servant Server running on localhost:8080"
  run 8080 $ serve (Proxy :: Proxy API) server
--startServer = run 8080 test
