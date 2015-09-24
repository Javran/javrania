{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Hakyll
import Data.Monoid
import Text.Pandoc
import Text.Highlighting.Kate.Styles

-- TODO: list all tags to a new page?
-- TODO: store static files under some specific directory
-- TODO: document dir structure

main :: IO ()
main = hakyllWith myConfig myRules

myRules :: Rules ()
myRules = do
    -- File stored in static-files are copied directly into dest directory
    -- directory structures are preserved.
    match "static-files/**" $ do
        let removeLeading = drop (length ("static-files/" :: String))
        route (customRoute (removeLeading . toFilePath))
        compile copyFileCompiler

    -- CSS files are compressed before being moved into dest directory
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- about page
    match "about.markdown" $ do
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
