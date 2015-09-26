{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Server where

import qualified DB.Query as Q
import qualified DB.Table.Post as P
import qualified Html.Post as HP

import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans (liftIO)
import Network.Wai (Application)
import Servant.HTML.Lucid (HTML)

import Servant

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
    posts = liftIO Q.runPostByAccountId
    newpost :: P.Body -> EitherT ServantErr IO [P.Post]
    newpost x = liftIO Q.runPostByAccountId

app :: Application
app = serve myApi server
