{-# LANGUAGE GADTs #-}

module Blog.Build.Render (
    markdownToHtml
) where

import qualified Data.Text       as T
import qualified Text.Pandoc     as Pan

readerOptions :: Pan.ReaderOptions
readerOptions = Pan.def
  { Pan.readerExtensions = Pan.enableExtension Pan.Ext_raw_html
  . Pan.enableExtension Pan.Ext_strikeout
  $ Pan.readerExtensions Pan.def
  }

markdownToHtml :: String -> Maybe String
markdownToHtml md = case htmlOrError of
  Left _     -> Nothing
  Right html -> Just (T.unpack html)
  where
    htmlOrError = Pan.runPure $ do
      doc <- Pan.readMarkdown readerOptions $ T.pack md
      Pan.writeHtml5String Pan.def doc
