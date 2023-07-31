{-# LANGUAGE OverloadedStrings #-}

module Blog.Component.Profile (_DEFAULT, defaultProfile, Profile) where

import qualified Blog.Component              as BC
import qualified Blog.Setup.Load             as L
import qualified Text.Blaze.Html5            as H hiding (main)
import qualified Text.Blaze.Html5.Attributes as A

_STYLE :: H.Attribute
_STYLE = A.style $ mconcat
  [ "background-color: black;"
  , "color: white;"
  , "padding: 10px;"
  , "width: 375px;"
  , "margin: 15px;"
  , "border-radius: 15px;"
  , "font-family: 'Courrier new', monospace;"
  , "font-size: 12px;"
  , "text-align: justify;"
  , "display: flex;"
  , "flex-direction: column;"
  , "align-items: center;"
  , "flex-grow: 0;"
  , "flex-shrink: 0"
  ]

_DEFAULT :: String
_DEFAULT = "---\ntype: ProfileInfo\n\n\n---\n# Hello, World!\nMy name is Bloggy Blogger and I'm writing a new blog."

newtype Profile = Profile { content :: String }
  deriving (Eq, Show)

instance BC.Component Profile where
  render p = H.div H.! _STYLE $ do
    H.preEscapedString $ content p

  load element = Profile {
    content =
      case L.markdownToHtml $ L.body element of
        Just e -> e
        _      -> ""
  }

defaultProfile :: L.Element
defaultProfile = L.empty

