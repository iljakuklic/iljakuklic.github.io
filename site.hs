--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>), mconcat)
import           Hakyll
import           Text.Pandoc.Options
import           Control.Applicative

-------------------------------------------------------------------------------

-- Pandoc config

pandocReadCfg = defaultHakyllReaderOptions {
    readerTabStop = 4
  }

pandocWriteCfg = defaultHakyllWriterOptions {
    writerTabStop = 4,
    writerHTMLMathMethod = MathJax "",
    writerIdentifierPrefix = "h-",
    writerHtml5 = True,
    writerHighlight = False
  }

myPandocCompiler = pandocCompilerWithTransformM pandocReadCfg pandocWriteCfg return

-- constants
numPostsOnTitlePage = 3

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/**" $ do
        route $ gsubRoute "static/" (const "")
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- take numPostsOnTitlePage <$> (recentFirst =<< loadAll "posts/*")
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            fmap fixupQuotes <$> myPandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx

    match "templates/*" $ compile templateCompiler

fixupQuotes  ('&':'q':'u':'o':'t':';':rest) = '"' : fixupQuotes  rest
fixupQuotes  ('<':rest)                     = '<' : fixupQuotes' rest
fixupQuotes  (chr:rest)                     = chr : fixupQuotes  rest
fixupQuotes  ""                             = ""
fixupQuotes' ('>':rest)                     = '>' : fixupQuotes  rest
fixupQuotes' (chr:rest)                     = chr : fixupQuotes' rest
fixupQuotes' ""                             = ""

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat [
    teaserField "teaser" "content",
    dateField "date" "%e %B %Y",
    defaultContext
  ]
