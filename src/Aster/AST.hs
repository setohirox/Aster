module Aster.AST where

-- Aster のノード（文・タグ・テキストなど）
data Node
  = Element TagName [Modifier] [Attribute] [Node] -- <tag ...>...</tag>
  | Text String
  | VarRef String
  | Include FilePath
  | BlockDef String [Node]
  | BlockInsert String
  deriving (Show, Eq)

type TagName = String

data Modifier
  = ModClass String
  | ModId String
  deriving (Show, Eq)

data Attribute
  = AttrKeyValue String String -- attr="value"
  deriving (Show, Eq)
