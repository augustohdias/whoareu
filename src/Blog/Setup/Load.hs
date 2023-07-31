{-# LANGUAGE OverloadedStrings #-}

module Blog.Setup.Load (
  Element(header, body)
, Header(elementType, date, title, extra)
, ElementType(Post, Profile, Custom)
, getProfile
, getPosts
, empty
, extractElement
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
data Element = Element { header :: Header, body :: String }
  deriving (Show, Eq)

extractElement :: String -> Element
extractElement contents = case header of
  Right h -> Element {header=h, body=body}
  _       -> Element {header=Header {elementType=Post, date="01-01-01", title="algo errado", extra=[("",[])]}, body=""}
  where
    header = Par.parse parseHeader "" contents
    body = unlines . drop 5 . lines $ contents

filterByElementType :: ElementType -> [Element] -> [Element]
filterByElementType et = filter ((== et) . elementType . header)

getPosts :: [Element] -> [Element]
getPosts = filterByElementType Post

getProfile :: [Element] -> Maybe Element
getProfile l = maybeProfile $ filterByElementType Profile l
  where
    maybeProfile (p:_) = Just p
    maybeProfile []    = Nothing

empty :: Element
empty = Element {header=Header{elementType=Profile,title="",date="",extra=[("",[])]}, body="wada"}

--- Render

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

