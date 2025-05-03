{-# LANGUAGE OverloadedStrings #-}

module Server where

import Web.Scotty
import Control.Concurrent

runweb :: IO ()
runweb = scotty 8081 $
  get "/query/count/:start/:end" $ do
    _ <- liftIO $ threadDelay 10000000
    beam1 <- pathParam "start"
    beam2 <- pathParam "end"
    html $ mconcat ["<h1>Scotty, ", beam1, " me up,", beam2, "!</h1>"]



-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}
--
-- module Server where
--
-- import Data.Aeson
-- import Data.ByteString.Lazy.Char8 (pack)
-- import Data.Text.Lazy.Encoding (encodeUtf8)
-- import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A
-- -- import Text.Blaze.Renderer.Utf8 (renderHtml)
-- import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
-- import Web.Scotty
--
-- -- Data type for JSON response
-- data ApiResponse = ApiResponse { message :: String, items :: [Int] }
--
-- instance ToJSON ApiResponse where
--     toJSON (ApiResponse msg items) =
--         Data.Aeson.object ["message" .= msg, "items" .= items]
--
-- -- HTML Template using Blaze
-- homePage :: Html
-- homePage = H.docTypeHtml $ H.html $ do
--     H.head $ do
--         H.title "Haskell Web Server"
--     H.body $ do
--         H.h1 "Welcome from Haskell!"
--         H.p "This is a sample web server serving HTML and JSON."
--
-- main :: IO ()
-- main = scotty 3000 $ do
--     get "/" $ do
--         Web.Scotty.html $ renderHtml homePage
--
--     get "/api/data" $ do
--         let responseData = ApiResponse { message = "Hello from JSON!", items = [1, 2, 3] }
--         json responseData
--
--     notFound $ Web.Scotty.text "404 Not Found"
