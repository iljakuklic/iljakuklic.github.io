--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>), mconcat)
import           Hakyll
import           Text.Pandoc.Options
import           Control.Applicative
import qualified Data.Time          as Sys
import qualified System.Directory   as Sys
import qualified System.Environment as Sys
import qualified System.Exit        as Sys
import qualified System.Process     as Sys
import           System.FilePath((</>))

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

-- Hakyll config
deployCmd conf = do
	hasDeployDir <- Sys.doesDirectoryExist deployDir
	if hasDeployDir
		then runIn deployDir "git" ["pull"]
		else runIn rootDir "git" ["clone", gitUrl, deployDir]
	runIn rootDir "rsync" ["--delete", "-av", "--exclude=.git*", siteDir, deployDir]
	runIn deployDir "git" ["add", "."]
	t <- Sys.getCurrentTime
	runIn deployDir "git" ["commit", "-m", "Update from " <> show t]
	runIn deployDir "git" ["push"]
	return Sys.ExitSuccess
  where
	runIn dir cmd args = do
		let myProc = (Sys.proc cmd args) { Sys.cwd = Just dir }
		(_, _, _, hdl) <- Sys.createProcess myProc
		Sys.waitForProcess hdl
	gitUrl = "git@github.com:iljakuklic/iljakuklic.github.io.git"
	siteDir = destinationDirectory conf </> "."
	rootDir = providerDirectory conf
	deployDir = rootDir </> "_deploy"

config = defaultConfiguration { deploySite = deployCmd }

--------------------------------------------------------------------------------
main :: IO ()
main = do
	(action:_) <- Sys.getArgs
	let posts = case action of
		"watch" -> "posts/*" .||. "drafts/*"
		_       -> "posts/*"

	hakyllWith config $ do
		match "images/*" $ do
			route   idRoute
			compile copyFileCompiler

		match "static/**" $ do
			route $ gsubRoute "static/" (const "")
			compile copyFileCompiler

		match "css/*" $ do
			route   idRoute
			compile compressCssCompiler

		match (fromList ["about.md", "contact.md"]) $ do
			route   $ setExtension "html"
			compile $ myPandocCompiler
				>>= loadAndApplyTemplate "templates/default.html" defaultContext

		match posts $ do
			route $ setExtension "html"
			compile $ myPandocCompiler
				>>= saveSnapshot "content"
				>>= loadAndApplyTemplate "templates/post.html"    postCtx
				>>= loadAndApplyTemplate "templates/default.html" postCtx

		create ["archive.html"] $ do
			route idRoute
			compile $ do
				posts <- recentFirst =<< loadAll posts
				let archiveCtx =
						listField "posts" postCtx (return posts) <>
						constField "title" "Archives"            <>
						constField "pageclass" "subpages"        <>
						defaultContext

				makeItem ""
					>>= loadAndApplyTemplate "templates/archive.html" archiveCtx
					>>= loadAndApplyTemplate "templates/default.html" archiveCtx

		match "index.md" $ do
			route $ setExtension "html"
			compile $ do
				posts <- take numPostsOnTitlePage <$> (recentFirst =<< loadAll posts)
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
