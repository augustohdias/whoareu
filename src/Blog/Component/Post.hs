{-# LANGUAGE OverloadedStrings #-}

module Blog.Component.Post (_DEFAULT, Post, date, title, pathOf, hyperlinkOf, borrowStyle, load) where

import           Blog.Component
import qualified Blog.Setup.Load             as L
import           Prelude                     hiding (read)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

_DEFAULT :: String
_DEFAULT= "---\ntype: BlogPost\ndate: 29.07.23\ntitle: Post title\n---\n# Titulo\n\nWada wada wada wadawada wadawadawada wada a wada."

_STYLE :: H.Attribute
_STYLE = A.style $ mconcat
  [ "background-color: black;"
  , "color: white;"
  , "padding: 15px;"
  , "width: 600px;"
  , "margin: 15px;"
  , "border-radius: 10px;"
  , "font-family: 'Courrier new', monospace;"
  , "font-size: 12px;"
  , "text-align: justify;"
  , "display: flex;"
  , "flex-direction: column;"
  , "flex-grow: 0;"
  , "flex-shrink: 0;"
  ]

data Post = Post { html  :: String, date  :: String, title :: String }

instance Component Post where
  render p = H.div H.! _STYLE $ do
    H.preEscapedString $ html p
    H.a H.! A.href "../index.html" $ "<<<"
  load element = Post {
      html = htmlBody
    , date = L.date $ L.header element
    , title = L.title $ L.header element
    }
    where
      htmlBody = case L.markdownToHtml $ L.body element of
        Just v -> v
        _      -> ""

replaceSpaces :: String -> String
replaceSpaces = Prelude.map (\c -> if c == ' ' then '-' else c)

pathOf :: Post -> String
pathOf post = "./posts/" ++ date post ++ "-" ++ replaceSpaces (title post) ++ ".html"

hyperlinkOf :: Post -> H.Html
hyperlinkOf post = H.div H.! A.style "display: flex; justify-content: space-between; min-height: 20px; font-size: 12px; padding: 15px; margin: 15px;" $ do
    H.a H.! A.href (H.stringValue (pathOf post)) $ H.toHtml $ title post
    H.toHtml $ " :: " ++ date post

borrowStyle :: H.Html -> H.Html
borrowStyle = H.div H.! _STYLE

