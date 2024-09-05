--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Text.Pandoc
import Text.Pandoc.Options
import Hakyll.Web.Pandoc
import qualified Data.Set as S

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "js/*" .||. "css/fonts/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith readerOpts writerOpts
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx `mappend` postCtx)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

readerOpts :: ReaderOptions
readerOpts = defaultHakyllReaderOptions

writerOpts :: WriterOptions
writerOpts = defaultHakyllWriterOptions {
    writerHTMLMathMethod = MathJax ""
}

mathCtx :: Context a
mathCtx = field "katex" $ \item -> do
    katex <- getMetadataField (itemIdentifier item) "katex"
    return $ case katex of
                    Just "false" -> ""
                    Just "off" -> ""
                    _ -> "<link rel=\"stylesheet\" href=\"/css/katex.min.css\">\n\
                             \<script type=\"text/javascript\" src=\"/js/katex.min.js\"></script>\n\
                             \<script src=\"/js/auto-render.min.js\"></script>"