{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CMarkGFM.Simple.GhcSyntaxHighlighter (
  highlightHaskellWith,
) where

import CMarkGFM.Simple
import GHC.SyntaxHighlighter.Themed (ThemeProviderConfig, renderHaskellWith)

-- | Highlight all Haskell code blocks with @ghc-syntax-highlighter-themed@.
--
-- > import qualified GHC.SyntaxHighlighter.Themed.HighlightJS as HighlightJS
-- > renderHtml . fmap (highlightHaskellWith HighlightJS.config) . parse defaultSettings $ content
highlightHaskellWith :: ThemeProviderConfig -> Nodes -> Nodes
highlightHaskellWith config = mapNodes (fmap highlight)
  where
    highlight = \case
      NodeCodeBlock info code
        | info `elem` ["hs", "haskell"]
        , Just code' <- renderHaskellWith config code -> NodeHtmlBlock code'
      node -> node
