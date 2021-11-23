{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- cabal:
build-depends: base, haskell-gi-base, gi-gtk == 3.0.*
-}

-- https://haskell-at-work.com/episodes/2018-11-13-gtk-programming-with-haskell.html
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (Text, pack)

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req

makeRequest :: IO String
makeRequest = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "foo" .= (10 :: Int),
            "bar" .= (20 :: Int)
          ]
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      (https "httpbin.org" /: "post") -- safe by construction URL
      (ReqBodyJson payload) -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      mempty -- query params, headers, explicit port number, etc.
  liftIO $ return (show (responseBody r :: Value))

main :: IO ()
main =  do
  Gtk.init Nothing

  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit

  button <- new Gtk.Button [ #label := "Click me!" ]

  on button #clicked (set button [ #sensitive := False,
                                   #label := "?" ])

  #add win button

  #resize win 640 480
  #showAll win

  Gtk.main
