{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Server where

import qualified DB.Query as Q
import qualified DB.Table.Post as P
import qualified HTML.Post as HP

import Control.Monad.Trans (liftIO)
import Network.Wai (Application)
import Servant.HTML.Lucid (HTML)

import Servant

type PostAPI = "posts" :> Get '[JSON, HTML] [P.Post]

postAPI :: Proxy PostAPI
postAPI = Proxy

server :: Server PostAPI
server = liftIO Q.runPostByAccountId

app :: Application
app = serve postAPI server
