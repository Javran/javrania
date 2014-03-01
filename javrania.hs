{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import           Hakyll
import           Data.Monoid ((<>), mconcat)
import           Text.Pandoc
import           Text.Highlighting.Kate.Styles

main :: IO ()
main = hakyllWith myConfig myRules

myRules :: Rules ()
myRules = do
    -- copy files that does not need to convert
    match "favicon.ico" $ do
        route idRoute
        compile copyFileCompiler

    match "assets/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- about page
    match (fromList ["about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompilerAbout
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerX
            >>= loadAndApplyTemplate "templates/post.html"       (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post-frame.html" (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html"    (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtxWithTags tags) (return posts)
                 <> constField "title" "Archives"
                 <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtxWithTags tags) (return posts)
                 <> constField "title" "Welcome"
                 <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title
                 <> listField "posts" (postCtxWithTags tags) (return posts)
                 <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "404.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompilerX
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    createFeed "atom.xml" renderAtom
    createFeed "rss.xml"  renderRss

createFeed :: Identifier
            -> (  FeedConfiguration
               -> Context String
               -> [Item String]
               -> Compiler (Item String))
            -> Rules ()
createFeed path renderXXX = create [ path ] $ do
    route idRoute
    compile $ do
        let feedCtx = bodyField "description" <> defaultContext

        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
        renderXXX myFeedConfig feedCtx posts

myConfig :: Configuration
myConfig = defaultConfiguration
    { destinationDirectory = "site-generated"
    , providerDirectory = "site-source"
    , previewPort = 18989
    , deployCommand = "/bin/bash ./do-deploy.sh"
    }

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = mconcat
    [ dateField "date" "%Y-%m-%d"
    , tagsField "tags" tags
    , defaultContext
    ]

-- customized pandocCompiler with pygment color scheme
pandocCompilerX :: Compiler (Item String)
pandocCompilerX = pandocCompilerWith readerOpt writerOpt
    where readerOpt = defaultHakyllReaderOptions
          writerOpt = defaultHakyllWriterOptions
                      { writerHighlight  = True
                      , writerHighlightStyle = pygments
                      }

pandocCompilerAbout :: Compiler (Item String)
pandocCompilerAbout = pandocCompilerWith readerOpt writerOpt
    where readerOpt = defaultHakyllReaderOptions
          writerOpt = defaultHakyllWriterOptions
                      { writerHighlight  = True
                      , writerTableOfContents = True
                      , writerStandalone = True
                      , writerTemplate = unlines [ "$toc$"
                                                 , "$body$" ]
                      , writerHighlightStyle = pygments
                      }

myFeedConfig :: FeedConfiguration
myFeedConfig = FeedConfiguration
    { feedTitle       = "Javrania"
    , feedDescription = "Javran's blog"
    , feedAuthorName  = "Javran Cheng"
    , feedAuthorEmail = "Javran.c+fromBlogFeed@gmail.com"
    , feedRoot        = "http://javran.github.io/"
    }
