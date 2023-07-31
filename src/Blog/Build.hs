{-# LANGUAGE OverloadedStrings #-}

module Blog.Build (build) where

import           Blog.Component                  (Component (load, render))
import           Blog.Component.Post             (hyperlinkOf)
import qualified Blog.Component.Post             as Post
import qualified Blog.Component.Profile          as Profile
import qualified Blog.Setup.Load                 as L (Element, extractElement,
                                                       getPosts, getProfile)
import           Control.Exception               (IOException, try)
import qualified Data.List                       as DL
import           System.Directory                (listDirectory)
import           System.FilePath                 ((</>))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H hiding (main)
import qualified Text.Blaze.Html5.Attributes     as A hiding (name)

_STYLE :: H.Attribute
_STYLE = A.style $ mconcat
  [ "min-height: 100vh;"
  , "max-width: 100vw;"
  , "overflow-x: hidden;"
  , "display: flex;"
  , "align-items: flex-start;"
  , "cursor: url(https://cur.cursors-4u.net/symbols/sym-1/sym86.cur), auto;"
  ]

_PROFILE_BOX_STYLE :: H.Attribute
_PROFILE_BOX_STYLE = A.style $ mconcat
  [ "display: inline-block;"
  , "max-height: 100%;"
  , "justify-content: center;"
  , "max-width: 100vw;"
  , "overflow: hidden"
  ]


_POSTS_BOX_STYLE :: H.Attribute
_POSTS_BOX_STYLE = A.style $ mconcat
  [ "display: flex;"
  , "flex-direction: column;"
  , "justify-content: center;"
  , "max-width: 100vw;"
  , "overflow: hidden"
  ]

listMarkdownFiles :: [FilePath] -> IO [FilePath]
listMarkdownFiles [] = return []
listMarkdownFiles (x:xs) = do
  result <- try (listDirectory x) :: IO (Either IOException [FilePath])
  case result of
    Left _ -> listMarkdownFiles xs
    Right names -> do
      let mdFiles = filter (".md" `DL.isSuffixOf`) (Prelude.map (x </>) names)
      rest <- listMarkdownFiles xs
      return $ mdFiles ++ rest

extractElements :: IO (L.Element, [L.Element])
extractElements = do
  files <- listMarkdownFiles ["./profile", "./posts"]
  putStrLn "Files found: "
  mapM_ putStrLn files
  contentsList <- mapM readFile files
  let elements = map L.extractElement contentsList
  let posts = L.getPosts elements
  let maybeProfile = L.getProfile elements
  case maybeProfile of
    Just profile -> return (profile, posts)
    _            -> return (Profile.defaultProfile, posts)

-- Assembly the entire blog

build :: IO ()
build = do
  elements <- extractElements
  let (profile', posts') = elements
  let profile = render (load profile' :: Profile.Profile)
  let posts = Prelude.map load posts' :: [Post.Post]
  writePostsPages profile posts
  writeIndexPage profile posts
  where
    writeIndexPage profile posts = do
      let links = mconcat $ Prelude.map hyperlinkOf posts
      let page = renderPage profile (Post.borrowStyle links)
      writeFile "index.html" page

    writePostsPages :: H.Html -> [Post.Post] -> IO ()
    writePostsPages profile = mapM_ (write profile)

    write :: H.Html -> Post.Post -> IO ()
    write profile post = writeFile (Post.pathOf post) (renderPage profile (render post))

-- Render a page

renderPage :: H.Html -> H.Html -> String
renderPage profileContent bodyContent = renderHtml $ H.docTypeHtml $ do
  H.head $ do
    H.style $ mconcat [H.preEscapedToHtml (
        "a:link {color: white;}" ++
        "a:visited {color: white;}" ++
        "a:hover {color: grey;}" ++
        "a:active {color: grey;}"
        )]
    H.title "who am i?"
  H.body H.! A.style ( mconcat
              [ "background-image: url(https://augustohdias.github.io/whoami/resources/background/bg_0.jpg);"
              , "background-size: cover;"
              , "background-attachment: fixed;"
              ]) $ do
    H.div H.! _STYLE $ do
      H.div H.! _PROFILE_BOX_STYLE  $ profileContent
      H.div H.! _POSTS_BOX_STYLE    $ bodyContent

