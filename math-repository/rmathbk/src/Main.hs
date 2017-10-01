{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Handlers.AddHandler as AddHandler

main :: IO ()
main = quickHttpServe site

staticdir = "src/static"

site :: Snap ()
site =
    ifTop (writeBS "hello w0rld") <|>
    route [ ("foo", writeBS "bwr")
          , ("echo/:echoparam", echoHandler)
          , ("create", createHandler)
          , ("add", AddHandler.add)
          ] <|>
    dir "static" (serveDirectory staticdir)

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

createHandler = do
   --writeBS "hey"
   sendFile (staticdir++"/html/create.html")
