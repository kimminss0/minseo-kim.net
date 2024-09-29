-- vim: sw=4
{-# LANGUAGE OverloadedStrings #-}
import           System.FilePath
import           Hakyll
import           Text.Pandoc.Options


main :: IO ()
main = hakyll $ do
    match "static/*" $ do
        route $ gsubRoute "static/" (const "")
        compile copyFileCompiler

    match ("images/*" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route $ setExtension "html" `composeRoutes` appendIndex
        compile $ pandocCustomCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html" `composeRoutes` appendIndex

        compile $ pandocCustomCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts.html"] $ do
        route $ idRoute `composeRoutes` appendIndex
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"               `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"   archiveCtx
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


appendIndex :: Routes
appendIndex = customRoute $
    (\(p, e) -> p </> "index" <.> e) . splitExtension . toFilePath


dropIndexHtml :: String -> Context a
dropIndexHtml key = mapContext transform (urlField key) where
    transform url = case splitFileName url of
        (p, "index.html") -> takeDirectory p
        _                 -> url


postCtx :: Context String
postCtx =
    dateField "date" "%F" `mappend` -- format defined at Data.Time.Format
    dropIndexHtml "url"   `mappend`
    defaultContext


pandocCustomCompiler :: Compiler (Item String)
pandocCustomCompiler = pandocCompilerWith readerOpts writerOpts
  where
    readerOpts = defaultHakyllReaderOptions
    writerOpts = defaultHakyllWriterOptions {
        writerHTMLMathMethod = MathJax ""
    }
