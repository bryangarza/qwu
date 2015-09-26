{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | An example which should always compile and demonstrates \"real\"
-- code.

module Html.Base where

import Control.Monad
import Data.Functor.Identity
import Lucid

demo :: Monad m => HtmlT m a -> HtmlT m a
demo body =
  doctypehtml_
    (do head_ (do meta_ [charset_ "utf-8"]
                  meta_ [name_ "viewport"
                        ,content_ "width=device-width, initial-scale=1"]
                  link_ [href_ "//fonts.googleapis.com/css?family=Open+Sans"
                        ,rel_ "stylesheet"
                        ,type_ "text/css"]
                  link_ [href_ "//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.1.0/css/bootstrap.min.css"
                        ,rel_ "stylesheet"
                        ,type_ "text/css"]
                  title_ "Qwu")
        body_ (div_ [class_ "container"]
                    (do h1_ "Qwu"
                        t_ [class_ "posts"] body)))
  where t_ :: Term a r
           => a -> r
        t_ = termWith "p" [class_ " t "]
