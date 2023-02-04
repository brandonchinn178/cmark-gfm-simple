{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CMarkGFM.Simple (
  -- * Parsing
  parse,

  -- * Rendering
  renderMarkdown,
  renderHtml,
  nodeToHtml,

  -- * Settings
  Settings (..),
  defaultSettings,

  -- * AST
  Nodes (..),
  Node (..),
  WithPos (..),
  PosInfo (..),
  mapNodes,
  foldNodes,
  overNodes,

  -- * Re-exports
  ListAttributes (..),
  ListType (..),
  DelimType (..),
  TableCellAlignment (..),
) where

import CMarkGFM (
  DelimType (..),
  ListAttributes (..),
  ListType (..),
  PosInfo (..),
  TableCellAlignment (..),
 )
import qualified CMarkGFM
import Data.Text (Text)
import qualified Data.Text as Text

data WithPos a = WithPos
  { getPosInfo :: Maybe PosInfo
  , unPos :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Nodes = Nodes {unNodes :: [WithPos (Node Nodes)]}
  deriving (Show, Eq)

data Node nodes
  = NodeThematicBreak
  | NodeParagraph nodes
  | NodeBlockQuote nodes
  | NodeHtmlBlock Text
  | NodeCodeBlock Text Text
  | NodeHeading Int nodes
  | NodeList ListAttributes [WithPos nodes]
  | NodeText Text
  | NodeSoftBreak
  | NodeLineBreak
  | NodeHtmlInline Text
  | NodeCode Text
  | NodeEmph nodes
  | NodeStrong nodes
  | NodeLink Text Text nodes
  | NodeImage Text Text nodes
  | NodeStrikethrough nodes
  | NodeTable [TableCellAlignment] (WithPos [WithPos nodes]) [WithPos [WithPos nodes]]
  | NodeFootnoteReference nodes
  | NodeFootnoteDefinition nodes
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Transform all nodes with the given function
--
-- ==== __Example__
--
-- For example, to remove all bold styling in the entire Markdown document:
--
-- > let stripBold = \case
-- >       WithPos _ (NodeEmph node) -> node
-- >       node -> node
-- >  in mapNodes stripBold nodes
mapNodes :: (WithPos (Node Nodes) -> WithPos (Node Nodes)) -> Nodes -> Nodes
mapNodes f = overNodes $ \go -> Nodes . map (fmap go . f)

-- | Recursively collect nodes with the given function.
--
-- ==== __Examples__
--
-- TODO: pure example
--
-- This function can also be used for effectful computations.
-- For example, to collect all image URLs:
--
-- > let tellImageURL = \case
-- >       WithPos _ (NodeImage url _ _) -> tell [url]
-- >       WithPos _ node -> sequence_ node
-- >  in execWriterT $ foldNodes tellImageURL nodes :: [Text]
foldNodes :: ([WithPos (Node a)] -> a) -> Nodes -> a
foldNodes f = overNodes $ \go -> f . map (fmap go)

-- | The most general traversable over a 'Nodes'.
--
-- Can be used to implement map, fold, mapM, foldM, and more.
overNodes :: ((Node Nodes -> Node a) -> [WithPos (Node Nodes)] -> a) -> Nodes -> a
overNodes f = f goNode . unNodes
  where
    go = overNodes f
    fmap2 = fmap . fmap
    goNode = \case
      NodeThematicBreak -> NodeThematicBreak
      NodeParagraph nodes -> NodeParagraph (go nodes)
      NodeBlockQuote nodes -> NodeBlockQuote (go nodes)
      NodeHtmlBlock t -> NodeHtmlBlock t
      NodeCodeBlock info t -> NodeCodeBlock info t
      NodeHeading level nodes -> NodeHeading level (go nodes)
      NodeList attrs nodes -> NodeList attrs (fmap2 go nodes)
      NodeText t -> NodeText t
      NodeSoftBreak -> NodeSoftBreak
      NodeLineBreak -> NodeLineBreak
      NodeHtmlInline t -> NodeHtmlInline t
      NodeCode t -> NodeCode t
      NodeEmph nodes -> NodeEmph (go nodes)
      NodeStrong nodes -> NodeStrong (go nodes)
      NodeLink url title nodes -> NodeLink url title (go nodes)
      NodeImage url title nodes -> NodeImage url title (go nodes)
      NodeStrikethrough nodes -> NodeStrikethrough (go nodes)
      NodeTable alignments headNodes rowNodes -> NodeTable alignments (fmap (fmap2 go) headNodes) (fmap2 (fmap2 go) rowNodes)
      NodeFootnoteReference nodes -> NodeFootnoteReference (go nodes)
      NodeFootnoteDefinition nodes -> NodeFootnoteDefinition (go nodes)

{----- Settings -----}

data Settings = Settings
  { includeSourcePos :: Bool
  , allowUnsafe :: Bool
  , softToHardBreaks :: Bool
  , useSmart :: Bool
  , enableFootnotes :: Bool
  , enableStrikethrough :: Bool
  , enableTable :: Bool
  , enableAutolink :: Bool
  , enableTagFilter :: Bool
  , enableTaskList :: Bool
  , lineLength :: Maybe Int
  }

defaultSettings :: Settings
defaultSettings =
  Settings
    { includeSourcePos = False
    , allowUnsafe = False
    , softToHardBreaks = False
    , useSmart = False
    , enableFootnotes = True
    , enableStrikethrough = True
    , enableTable = True
    , enableAutolink = True
    , enableTagFilter = True
    , enableTaskList = True
    , lineLength = Nothing
    }

{----- Parsing -----}

parse :: Settings -> Text -> WithPos Nodes
parse settings = toNodes . CMarkGFM.commonmarkToNode (toOpts settings) (toExts settings)

{----- Rendering -----}

renderMarkdown :: Settings -> WithPos Nodes -> Text
renderMarkdown settings = CMarkGFM.nodeToCommonmark (toOpts settings) (lineLength settings) . fromNodes

renderHtml :: WithPos Nodes -> Text
renderHtml = foldNodes (Text.concat . map (nodeToHtml . unPos)) . unPos

nodeToHtml :: Node Text -> Text
nodeToHtml = \case
  NodeThematicBreak -> "<hr />"
  NodeParagraph inner -> tagged "p" inner
  NodeBlockQuote inner -> tagged "blockquote" inner
  NodeHtmlBlock t -> t
  NodeCodeBlock lang t -> tagged "pre" . tagged' "code" [("class", "language-" <> lang)] $ t
  NodeHeading level inner -> tagged ("h" <> Text.pack (show level)) inner
  NodeList ListAttributes{..} inner ->
    let container =
          case listType of
            BULLET_LIST -> "ul"
            ORDERED_LIST -> "ol"
     in tagged container . Text.concat . map (tagged "li" . unPos) $ inner
  NodeText t -> escape t
  NodeSoftBreak -> "\n"
  NodeLineBreak -> "<br />"
  NodeHtmlInline t -> t
  NodeCode t -> tagged "code" t
  NodeEmph inner -> tagged "em" inner
  NodeStrong inner -> tagged "strong" inner
  NodeLink url title inner -> tagged' "a" [("href", url), ("title", title)] inner
  NodeImage url title inner -> tagged' "img" [("src", url), ("alt", title)] inner
  NodeStrikethrough inner -> tagged "del" inner
  NodeTable alignments headNodes rowNodes ->
    let mkCells tag row =
          Text.unlines $
            zipWith
              (\alignment cell -> tagged' tag (mkStyle alignment) (unPos cell))
              alignments
              (unPos row)
        mkStyle = \case
          NoAlignment -> []
          LeftAligned -> [("style", "text-align: left;")]
          CenterAligned -> [("style", "text-align: center;")]
          RightAligned -> [("style", "text-align: right;")]
     in tagged "table" . Text.unlines . map (tagged "tr") $ mkCells "th" headNodes : map (mkCells "td") rowNodes
  -- https://github.com/kivikakk/cmark-gfm-hs/issues/28
  NodeFootnoteReference _ -> error "Cannot render footnotes"
  NodeFootnoteDefinition _ -> error "Cannot render footnotes"
  where
    tagged tag = tagged' tag []
    tagged' tag attrs t =
      Text.concat
        [ "<" <> Text.unwords (tag : map (\(attr, v) -> attr <> "='" <> v <> "'") attrs) <> ">"
        , t
        , "</" <> tag <> ">"
        ]

    escape =
      Text.replace "\"" "&quot;"
        . Text.replace "<" "&lt;"
        . Text.replace ">" "&gt;"
        . Text.replace "&" "&amp;"

{----- Conversions to cmark-gfm -----}

toOpts :: Settings -> [CMarkGFM.CMarkOption]
toOpts Settings{..} =
  concat
    [ [CMarkGFM.optSourcePos | includeSourcePos]
    , [CMarkGFM.optHardBreaks | softToHardBreaks]
    , [CMarkGFM.optSmart | useSmart]
    , [CMarkGFM.optUnsafe | allowUnsafe]
    , [CMarkGFM.optFootnotes | enableFootnotes]
    ]

toExts :: Settings -> [CMarkGFM.CMarkExtension]
toExts Settings{..} =
  concat
    [ [CMarkGFM.extStrikethrough | enableStrikethrough]
    , [CMarkGFM.extTable | enableTable]
    , [CMarkGFM.extAutolink | enableAutolink]
    , [CMarkGFM.extTagfilter | enableTagFilter]
    , [CMarkGFM.extTaskList | enableTaskList]
    ]

toNodes :: CMarkGFM.Node -> WithPos Nodes
toNodes = \case
  CMarkGFM.Node pos CMarkGFM.DOCUMENT nodes -> WithPos pos $ go nodes
  CMarkGFM.Node _ nodeType children -> unexpected nodeType children
  where
    go nodes =
      Nodes
        [ WithPos pos $ toNode (nodeType, children)
        | CMarkGFM.Node pos nodeType children <- nodes
        ]

    fmap2 = fmap . fmap
    toNode = \case
      (CMarkGFM.THEMATIC_BREAK, []) -> NodeThematicBreak
      (CMarkGFM.PARAGRAPH, children) -> NodeParagraph $ go children
      (CMarkGFM.BLOCK_QUOTE, children) -> NodeBlockQuote $ go children
      (CMarkGFM.HTML_BLOCK t, []) -> NodeHtmlBlock t
      (CMarkGFM.CODE_BLOCK info t, []) -> NodeCodeBlock info t
      (CMarkGFM.HEADING level, children) -> NodeHeading level $ go children
      (CMarkGFM.LIST attrs, children)
        | Just items <- mapM getItem children ->
            NodeList attrs $ fmap2 go items
      (CMarkGFM.TEXT t, []) -> NodeText t
      (CMarkGFM.SOFTBREAK, []) -> NodeSoftBreak
      (CMarkGFM.LINEBREAK, []) -> NodeLineBreak
      (CMarkGFM.HTML_INLINE t, []) -> NodeHtmlInline t
      (CMarkGFM.CODE t, []) -> NodeCode t
      (CMarkGFM.EMPH, children) -> NodeEmph $ go children
      (CMarkGFM.STRONG, children) -> NodeStrong $ go children
      (CMarkGFM.LINK url title, children) -> NodeLink url title $ go children
      (CMarkGFM.IMAGE url title, children) -> NodeImage url title $ go children
      (CMarkGFM.STRIKETHROUGH, children) -> NodeStrikethrough $ go children
      (CMarkGFM.TABLE alignments, headerRow : rows)
        | Just headNodes <- getTableRow headerRow
        , Just rowNodes <- traverse getTableRow rows ->
            NodeTable alignments (fmap (fmap2 go) headNodes) (fmap2 (fmap2 go) rowNodes)
      (CMarkGFM.FOOTNOTE_REFERENCE, children) -> NodeFootnoteReference $ go children
      (CMarkGFM.FOOTNOTE_DEFINITION, children) -> NodeFootnoteDefinition $ go children
      (nodeType, children) -> unexpected nodeType children

    getItem = \case
      CMarkGFM.Node pos CMarkGFM.ITEM item -> Just (WithPos pos item)
      _ -> Nothing
    getTableRow = \case
      CMarkGFM.Node pos CMarkGFM.TABLE_ROW row
        | Just nodes <- mapM getTableCell row ->
            Just (WithPos pos nodes)
      _ -> Nothing
    getTableCell = \case
      CMarkGFM.Node pos CMarkGFM.TABLE_CELL cell -> Just (WithPos pos cell)
      _ -> Nothing

    unexpected nodeType children =
      error . unlines $
        [ "Unexpected cmark-gfm result."
        , "This is probably a bug, please report an issue at https://github.com/brandonchinn178/cmark-gfm-simple."
        , show nodeType
        , show children
        ]

fromNodes :: WithPos Nodes -> CMarkGFM.Node
fromNodes (WithPos pos0 nodes0) = CMarkGFM.Node pos0 CMarkGFM.DOCUMENT $ foldNodes go nodes0
  where
    go =
      map
        ( \(WithPos pos node) ->
            let (nodeType, children) = fromNode node
             in CMarkGFM.Node pos nodeType children
        )

    fromNode = \case
      NodeThematicBreak -> (CMarkGFM.THEMATIC_BREAK, [])
      NodeParagraph nodes -> (CMarkGFM.PARAGRAPH, nodes)
      NodeBlockQuote nodes -> (CMarkGFM.BLOCK_QUOTE, nodes)
      NodeHtmlBlock t -> (CMarkGFM.HTML_BLOCK t, [])
      NodeCodeBlock info t -> (CMarkGFM.CODE_BLOCK info t, [])
      NodeHeading level nodes -> (CMarkGFM.HEADING level, nodes)
      NodeList attrs items ->
        ( CMarkGFM.LIST attrs
        , [ CMarkGFM.Node pos CMarkGFM.ITEM item
          | WithPos pos item <- items
          ]
        )
      NodeText t -> (CMarkGFM.TEXT t, [])
      NodeSoftBreak -> (CMarkGFM.SOFTBREAK, [])
      NodeLineBreak -> (CMarkGFM.LINEBREAK, [])
      NodeHtmlInline t -> (CMarkGFM.HTML_INLINE t, [])
      NodeCode t -> (CMarkGFM.CODE t, [])
      NodeEmph nodes -> (CMarkGFM.EMPH, nodes)
      NodeStrong nodes -> (CMarkGFM.STRONG, nodes)
      NodeLink url title nodes -> (CMarkGFM.LINK url title, nodes)
      NodeImage url title nodes -> (CMarkGFM.IMAGE url title, nodes)
      NodeStrikethrough nodes -> (CMarkGFM.STRIKETHROUGH, nodes)
      NodeTable alignments headNodes rowNodes ->
        ( CMarkGFM.TABLE alignments
        , [ CMarkGFM.Node rowPos CMarkGFM.TABLE_ROW $
            [ CMarkGFM.Node cellPos CMarkGFM.TABLE_CELL cell
            | WithPos cellPos cell <- row
            ]
          | WithPos rowPos row <- headNodes : rowNodes
          ]
        )
      NodeFootnoteReference nodes -> (CMarkGFM.FOOTNOTE_REFERENCE, nodes)
      NodeFootnoteDefinition nodes -> (CMarkGFM.FOOTNOTE_DEFINITION, nodes)
