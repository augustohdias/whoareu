{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Blog.Setup.Load (
  Element (..)
, Header (..)
, ElementType (..)
, Rendered (..)
, Renderable (..)
, markdownToHtml
) where

import qualified Data.Text          as T
import qualified Text.Pandoc        as Pan
import qualified Text.Parsec        as Par
import qualified Text.Parsec.String as Par.String (Parser)

data ElementType = Post | Profile | Custom 
  deriving (Show, Eq)

data Header = Header { elementType:: ElementType, date :: String, title :: String, extra :: [(String, [String])] }
  deriving (Show, Eq)

data Element a where
  ElementPost     :: { body :: String, header :: Header } -> Element ElementType
  ElementProfile  :: { body :: String, header :: Header } -> Element ElementType

--- Render

data Rendered a where
  RenderedPost    :: { html :: String } -> Rendered (Element ElementType)
  RenderedProfile :: { html :: String } -> Rendered (Element ElementType)

class Renderable a where
  render :: a -> Rendered a

instance Renderable (Element ElementType) where
  render :: Element ElementType -> Rendered (Element ElementType)
  render (ElementPost body _) = case markdownToHtml body of
    Just html -> RenderedPost { html = html }
    _ -> RenderedPost { html = "" }

  render (ElementProfile body _) = case markdownToHtml body of
    Just html -> RenderedProfile { html = html }
    _ -> RenderedProfile {html = "" }

readerOptions :: Pan.ReaderOptions
readerOptions = Pan.def { Pan.readerExtensions = Pan.enableExtension Pan.Ext_raw_html .Pan.enableExtension Pan.Ext_strikeout $ Pan.readerExtensions Pan.def }

markdownToHtml :: String -> Maybe String
markdownToHtml md =
    case Pan.runPure $ do
        doc <- Pan.readMarkdown readerOptions $ T.pack md
        Pan.writeHtml5String Pan.def doc
    of
        Left _     -> Nothing
        Right html -> Just (T.unpack html)

--- Parser

parseHeader :: Par.String.Parser Header
parseHeader = do
    _ <- Par.string "---"
    _ <- Par.endOfLine
    _ <- Par.string "type:"
    fileType <- Par.manyTill Par.anyChar Par.endOfLine
    _ <- Par.string "date:"
    date <- Par.manyTill Par.anyChar Par.endOfLine
    _ <- Par.string "title:"
    title <- Par.manyTill Par.anyChar Par.endOfLine
 {--    _ <- Par.string "extra:"
    _ <- Par.char '['
    extra <- Par.sepBy parsePair (Par.char ',')
    _ <- Par.char ']'
--}
    _ <- Par.string "---"
    _ <- Par.endOfLine
    return $ Header (toElementType $ filter (/= ' ') fileType) (filter (/= ' ') date) title [("",[])]
    where
      toElementType s
        | s == "Post" = Post
        | s == "Profile" = Profile
        | otherwise = Custom

