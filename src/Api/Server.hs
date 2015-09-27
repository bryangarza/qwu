{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Server where

import           DB.Query (runPostByAccountId)
import qualified DB.Table.Post as P
import           Html.Post

import Control.Monad.Trans        (liftIO)
import Control.Monad.Trans.Either (EitherT)
import Network.Wai                (Application)
import Servant
import Servant.HTML.Lucid         (HTML)

type MyApi = "posts" :> Get '[JSON, HTML] [P.Post]
        -- takes Body type as JSON, returns [Post]
        :<|> "newpost" :> ReqBody '[JSON] P.Body :> Post '[JSON, HTML] [P.Post]

myApi :: Proxy MyApi
myApi = Proxy

server :: Server MyApi
server = posts
    :<|> newpost
  where
    posts :: EitherT ServantErr IO [P.Post]
    posts = liftIO runPostByAccountId
    newpost :: P.Body -> EitherT ServantErr IO [P.Post]
    newpost x = liftIO runPostByAccountId

app :: Application
app = serve myApi server
