--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Slug
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Text.Pandoc.Options
import           Text.Pandoc.Extensions
import           Text.Pandoc.Highlighting 
--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration{ destinationDirectory = "docs"}

main :: IO ()
main = hakyllWith config $ do
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

    match "posts/*" $ do
        route $ metadataRoute titleRoute
        compile $ pandocCompiler_
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Archives"            `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

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