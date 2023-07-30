{-# LANGUAGE OverloadedStrings #-}

module Blog.Component.Profile (_DEFAULT, build) where

import qualified Blog.Component              as BC
import qualified Blog.Setup.Template         as BT
import qualified Text.Blaze.Html5            as H hiding (main)
import qualified Text.Blaze.Html5.Attributes as A

_STYLE :: H.Attribute
_STYLE = A.style $ mconcat
  [ "background-color: black;"
  , "color: white;"
  , "padding: 10px;"
  , "width: 250px;"
  , "margin: 10px;"
  , "border-radius: 10px;"
  , "font-family: 'Courrier new', monospace;"
  , "font-size: 10px;"
  , "text-align: justify;"
  , "display: flex;"
  , "flex-direction: column;"
  , "align-items: center;"
  , "flex-grow: 0;"
  , "flex-shrink: 0"
  ]

_DEFAULT :: String
_DEFAULT = "---\ntype: ProfileInfo\n\n\n---\n# Hello, World!\nMy name is Bloggy Blogger and I'm writing a new blog."

newtype Profile = Profile { htmlContent :: String }

instance BC.Component Profile where
  renderHtml p = H.div H.! _STYLE $ H.preEscapedString $ htmlContent p
  parseTemplate template = Profile { htmlContent = BT.html template }

build :: IO H.Html
build = do
  maybeProfile    <- BC.loadFromFile "./profile/Info.md" :: IO (Maybe Profile)
  defaultProfile  <- BT.readFromString _DEFAULT
  case maybeProfile of
    Just profile  -> return $ BC.renderHtml profile
    Nothing -> return $ BC.renderHtml (BC.parseTemplate defaultProfile :: Profile)

