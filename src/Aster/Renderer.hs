module Aster.Renderer
  ( renderNode
  , renderNodes
  , renderDocument
  ) where

import           Aster.AST

------------------------------------------------------------
-- HTML エスケープ
------------------------------------------------------------

escapeHtml :: String -> String
escapeHtml = concatMap go
  where
    go '<'  = "&lt;"
    go '>'  = "&gt;"
    go '&'  = "&amp;"
    go '"'  = "&quot;"
    go '\'' = "&#39;"
    go c    = [c]

------------------------------------------------------------
-- 属性まわり
------------------------------------------------------------

renderModifiers :: [Modifier] -> String
renderModifiers mods =
  let
    -- id は最初の1個だけ採用
    ids = [i | ModId i <- mods]
    mIdAttr = case ids of
      []    -> ""
      (i:_) -> " id=\"" ++ escapeHtml i ++ "\""

    -- class は全部まとめて class="a b c"
    classes = [c | ModClass c <- mods]
    classAttr =
      if null classes
        then ""
        else " class=\"" ++ unwords (map escapeHtml classes) ++ "\""
  in
    mIdAttr ++ classAttr

renderAttr :: Attribute -> String
renderAttr (AttrKeyValue k v) =
  " " ++ k ++ "=\"" ++ escapeHtml v ++ "\""

renderAttributes :: [Attribute] -> String
renderAttributes attrs = concatMap renderAttr attrs

------------------------------------------------------------
-- Node → HTML
------------------------------------------------------------

renderNode :: Node -> String
renderNode (Text s) =
  escapeHtml s

renderNode (Element tag mods attrs children) =
  "<" ++ tag ++ renderModifiers mods ++ renderAttributes attrs ++ ">"
  ++ renderNodes children
  ++ "</" ++ tag ++ ">"

-- まだ VarRef / Include / Block 系は未実装なのでいったん無視
renderNode (VarRef _)      = ""
renderNode (Include _)     = ""
renderNode (BlockDef _ _)  = ""
renderNode (BlockInsert _) = ""

renderNodes :: [Node] -> String
renderNodes = concatMap renderNode

------------------------------------------------------------
-- ドキュメント全体（ラッパー）
------------------------------------------------------------

renderDocument :: [Node] -> String
renderDocument nodes = unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "  <head>"
  , "    <meta charset=\"utf-8\">"
  , "  </head>"
  , "  <body>"
  , renderNodes nodes
  , "  </body>"
  , "</html>"
  ]
