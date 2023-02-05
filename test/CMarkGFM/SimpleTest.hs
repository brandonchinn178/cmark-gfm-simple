{- AUTOCOLLECT.TEST -}
{-# LANGUAGE OverloadedStrings #-}

module CMarkGFM.SimpleTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import qualified CMarkGFM
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import Test.Tasty.Golden

import CMarkGFM.Simple

test_batch =
  [ goldenVsStringDiff ("[golden] " <> Text.unpack label) diff goldenFile $ do
    let refNodes = CMarkGFM.commonmarkToNode defaultOpts defaultExts input
    let nodes = parse defaultSettings input
    pure . Text.Lazy.encodeUtf8 . Text.Lazy.fromStrict . Text.unlines $
      [ "================= Input ================="
      , input
      , ""
      , "========= cmark-gfm-simple HTML ========="
      , renderHtml nodes
      , ""
      , "============ cmark-gfm HTML ============="
      , CMarkGFM.nodeToHtml defaultOpts defaultExts refNodes
      , ""
      , "======= cmark-gfm-simple Markdown ======="
      , renderMarkdown defaultSettings nodes
      , ""
      , "========== cmark-gfm Markdown ==========="
      , CMarkGFM.nodeToCommonmark defaultOpts Nothing refNodes
      , ""
      , "========== cmark-gfm-simple AST ========="
      , Text.pack (show nodes)
      , ""
      , "============= cmark-gfm AST ============="
      , Text.pack (show refNodes)
      ]
  | (label, input) <- testCases
  , let goldenFile = Text.unpack $ "test/goldens/" <> (Text.replace " " "-" . Text.toLower) label <> ".golden"
  ]
  where
    diff ref new = ["diff", "-u", ref, new]
    testCases =
      [ ("Thematic break", "a\n\n---\n\nb")
      , ("Paragraph", "a\n\nb")
      , ("Block quote", "> a\n> b")
      , ("HTML block", "<details>\n<summary>Click for details</summary>\n\n```\nstuff\n```\n</details>")
      , ("Code block", "```hs\nmain = putStrLn \"Hello world!\"\n```")
      , ("Code block raw", "```\nlog file\n```")
      , ("Heading", "# My title\n\ncontent")
      , ("Bullet list", "* item 1\n* item 2\n* item 3")
      , ("Numbered list", "1. item 1\n1. item 2\n1. item 3")
      , ("Numbered list with parens", "1) item 1\n2) item 2\n3) item 3")
      , ("Soft break", "a\nb\nc")
      , ("Line break", "a\\\nb  \nc")
      , ("HTML inline", "Some <mark>highlighted</mark> content")
      , ("Inline code", "Some `inline code` here.")
      , ("Emphasis", "Some *emphasized* content")
      , ("Bold", "Some **bold** content")
      , ("Link", "Some [linked](https://google.com) content")
      , ("Link with title", "Some [linked](https://example.com 'Example site') content")
      , ("Image", "An image:\n![](icon.svg)")
      , ("Image with info", "An image:\n![What a great icon](icon.svg 'A cool icon')")
      ]

test_todo = "strikethrough (https://github.com/kivikakk/cmark-gfm-hs/issues/29)"
test_todo = "table"
test_todo = "footnotes"
test_todo = "mapNodes"
test_todo = "foldNodes"
test_todo = "foldNodes effectful"


defaultOpts :: [CMarkGFM.CMarkOption]
defaultOpts = [CMarkGFM.optFootnotes]

defaultExts :: [CMarkGFM.CMarkExtension]
defaultExts =
  [ CMarkGFM.extStrikethrough
  , CMarkGFM.extTable
  , CMarkGFM.extAutolink
  , CMarkGFM.extTagfilter
  , CMarkGFM.extTaskList
  ]
