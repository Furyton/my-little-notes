--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import           Data.Monoid     ((<>))
import           Hakyll
import           Slug
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Text.Pandoc.Options
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting
-- import           Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = hakyllWith config $ do
    -- Static files
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.rst" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler_
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match "posts/*" $ do
        route   $ metadataRoute titleRoute
        compile $ pandocCompiler_
                >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
                -- >>= loadAndApplyTemplate "templates/content.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post tags
    -- tagsRules tags $ \tag pattern -> do
    --     let title = "Posts tagged " ++ tag

    --     -- Copied from posts, need to refactor
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll pattern
    --         let ctx = constField "title" title <>
    --                     listField "posts" (postCtx tags) (return posts) <>
    --                     defaultContext
    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/tag.html" ctx
    --             -- >>= loadAndApplyTemplate "templates/content.html" ctx
    --             >>= loadAndApplyTemplate "templates/default.html" ctx
    --             >>= relativizeUrls

    create ["tags.html"] $ do
        route idRoute
        let tagList = tagsMap tags
        compile $ do
            makeItem ""
              >>= loadAndApplyTemplate "templates/tag-list.html" (defaultCtxWithTags tags)
              >>= loadAndApplyTemplate "templates/default.html" defaultContext

    tagsRules tags $ \tag pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pat
            let postCtx = postCtxWithTags tags
                postsField = listField "posts" postCtx (pure posts)
                titleField = constField "title" ("Posts tagged \""++tag++"\"")
                indexCtx = postsField <> titleField <> defaultContext
            makeItem "" >>= loadAndApplyTemplate "templates/post-list.html" indexCtx
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= relativizeUrls

    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexContext =
                    listField "posts" (postCtxWithTags tags) (return posts) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                -- >>= loadAndApplyTemplate "templates/content.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%0Y-%m-%d" `mappend`
    defaultContext

postCtxWithTags  :: Tags -> Context String
postCtxWithTags  tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]

defaultCtxWithTags :: Tags -> Context String
defaultCtxWithTags tags = listField "tags" tagsCtx getAllTags         <>
                          defaultContext
  where getAllTags :: Compiler [Item (String, [Identifier])]
        getAllTags = mapM (pure . mkItem) $ tagsMap tags
          where mkItem :: (String, [Identifier]) -> Item (String, [Identifier])
                mkItem x@(t, _) = Item (tagsMakeId tags t) x
        tagsCtx :: Context (String, [Identifier])
        tagsCtx = listFieldWith "posts" postsCtx getPosts             <>
                  metadataField                                       <>
                  urlField "url"                                      <>
                  pathField "path"                                    <>
                  titleField "title"                                  <>
                  missingField
          where getPosts :: Item (String, [Identifier])
                         -> Compiler [Item String]
                getPosts (itemBody -> (_, is)) = mapM load is
                postsCtx :: Context String
                postsCtx = postCtxWithTags tags

config :: Configuration
config = defaultConfiguration{ destinationDirectory = "docs"}

pandocCompiler_ :: Compiler (Item String)
pandocCompiler_ =
    let
    mathExtensions =
        [ Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros
        ]
    codeExtensions =
        [ Ext_fenced_code_blocks
        , Ext_backtick_code_blocks
        , Ext_fenced_code_attributes
        ]
    newExtensions = foldr enableExtension defaultExtensions (mathExtensions <> codeExtensions)
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    writerOptions =
        defaultHakyllWriterOptions
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = MathJax ""
        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

titleRoute :: Metadata -> Routes
titleRoute = constRoute . fileNameFromTitle

fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle = 
    T.unpack . (`T.append` ".html") . toSlug . T.pack . getTitleFromMeta

getTitleFromMeta :: Metadata -> String
getTitleFromMeta =
    fromMaybe "no title" . lookupString "title"

-- postListTemplate :: Template
-- postListTemplate = readTemplate . renderHtml $ postListTemplateRaw

-- postListTemplateRaw :: Html
-- postListTemplateRaw =
--   ul $ do
--     "$for(posts)$"
--     li ! A.class_ "" $ do
--         "$date$: "
--         a ! href "$url$" $ "$title$"
--         p ! class_ "taglist" $ "$if(tags)$ Tags: $tags$ $endif$"
--     "$endfor$"

-- tagListTemplate = readTemplate . renderHtml $ tagListTemplateRaw

-- tagListTemplateRaw :: Html
-- tagListTemplateRaw =
--   ul $ do
--     "$for(tags)$"
--     li ! A.class_ "" $ do
--         a ! href "$url$" $ "$title$"
--         ul $ do
--           "$for(posts)$"
--           li ! A.class_ "" $ do
--             a ! href "$url$" $ "$title$"
--           "$endfor$"
--     "$endfor$"