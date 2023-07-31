{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Blog.Build.Element (Element(..), Post, Profile, Header(..), Rendered(..), Renderable(..)) where

import           Blog.Build.Render (markdownToHtml)

data Post
data Profile
data Page = Page {bio :: Rendered (Element Profile), content :: Rendered (Element Post)}

data Header a where
  PostHeader :: { date :: String, title :: String, extra :: [(String, [String])] } -> Header Post
  ProfileHeader :: {} -> Header Profile

data Element a where
  ElementPost     :: { postContent  :: String, postMetadata :: Header Post } -> Element Post
  ElementProfile  :: { profileBio   :: String, profileMetadata :: Header Profile } -> Element Profile

data Rendered a where
  RenderedPost    :: { postHtml     :: String, postHeader     :: Header Post } -> Rendered (Element Post)
  RenderedProfile :: { profileHtml  :: String, profileHeader  :: Header Profile } -> Rendered (Element Profile)
  RenderedPage    :: { pageHtml     :: String } -> Rendered Page

class Renderable a where
  render :: a -> Rendered a

instance Renderable (Element Post) where
  render (ElementPost body header) = case markdownToHtml body of
    Just html -> RenderedPost { postHtml = html, postHeader = header }
    _         -> RenderedPost {}

instance Renderable (Element Profile) where
  render (ElementProfile  body header) = case markdownToHtml body of
      Just html -> RenderedProfile { profileHtml = html, profileHeader = header }
      _         -> RenderedProfile {}
