{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{- cabal:
build-depends: base, haskell-gi-base, gi-gtk == 3.0.*
-}

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (Text, pack)
import Control.Monad.IO.Class

-- https://hackage.haskell.org/package/aeson-2.0.2.0/docs/Data-Aeson.html
import Data.Aeson
import Network.HTTP.Req

-- Explained here: https://hackage.haskell.org/package/req
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

makeRequestAndDraw :: Gtk.Button -> IO ()
-- shows how to do something and then change button
makeRequestAndDraw button = makeRequest >>= \text ->
    set button [#sensitive := False,
    -- https://hackage.haskell.org/package/text-1.2.5.0/docs/Data-Text.html
    --          V--- use pack to create the text object that GTK uses
      #label := pack text]

-- https://haskell-at-work.com/episodes/2018-11-13-gtk-programming-with-haskell.html
main :: IO ()
main =  do
  Gtk.init Nothing

  -- create window and specify what happens when closing
  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit

  -- create buttons
  button <- new Gtk.Button [ #label := "Click me!" ]

  -- on button press, calls function and modify button
  on button #clicked (makeRequestAndDraw button)

  -- add button to window
  #add win button
   
  -- default size is small, so we resize here
  #resize win 640 480

  -- before starting, should be full screen
  #showAll win
  
  Gtk.main

-- TODOS:
--  SEARCH/DEFINE/DELETE
-- 
--  > function/serde.pl for JSON format
--  > Use Aeson for JSON files
--
--  > server.pl for API + expected responses
--  > README.md also covers API
--  > Use Req for HTTP Requests
-- 
--  HTTP Endpoint/Method | Purpose
--  func/POST:    Define functions
--  func/GET:     Search for functions
--  func/DELETE:  Delete functions
--  type/POST:    Define types
--  type/GET:     Search for types
--  type/DELETE:  Delete types
--  trait/POST:   Define traits (eg. typeclasses)
--  trait/GET:    Search traits
--  trait/DELETE: Delete traits
--  > Search should be based on Paths (basically, use
--  > the same code, but remove some constraints which
--  > don't make sense)
--  > Delete takes a UUID (so you search, get uuids, and
--  > then delete them)
--  > Define should be done using JSON format in serde.pl
--  ---------------------------------------------------------
--  Save data (CTRL+S?)
--
--  > Only needs to save file from server
--  Load file
--  > Only needs to send file to server
-- ---------------------------------------------------------
--  Help information
--  > look at main.pl for some ideas
-- ---------------------------------------------------------
-- Paths
-- 
-- > want to be able to add constraints
-- eg. length (number)
-- string contraints (eg. regex)
-- input/output types
-- AND/OR constraints (eg. all of / one of these are true)
-- ---------------------------------------------------------
-- General quality of life / Optional ideas
-- 
-- make all commands easy to find and get help for
-- Make search box which as you type, shows results?
-- search history? should restore appropriate window
--
-- Make sure to start/close server
-- > maybe allow user to choose their own server?