{-# LANGUAGE OverloadedStrings #-}

module Aster.Parser
  ( Parser
  , parseAster
  ) where

import           Aster.AST
import           Control.Monad (void)
import           Data.Void (Void)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Parser 型
type Parser = Parsec Void Text

------------------------------------------------------------
-- 改行や空行も許可するバージョン
scn :: Parser ()
scn = L.space
        (void spaceChar)
        (L.skipLineComment "🌼")
        (L.skipBlockComment "🌼{" "}")

identifier :: Parser Text
identifier = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

------------------------------------------------------------
-- Modifier（#id, .class）
------------------------------------------------------------

modifier :: Parser Modifier
modifier =
      (ModId <$> (char '#' *> some alphaNumChar))
  <|> (ModClass <$> (char '.' *> some alphaNumChar))

modifiers :: Parser [Modifier]
modifiers = many modifier

------------------------------------------------------------
-- タグ行のパース
------------------------------------------------------------

tagLine :: Parser (TagName, [Modifier], Maybe String, Bool)
tagLine = do
  tag <- identifier
  mods <- modifiers
  hasChildren <- (True <$ char ':') <|> pure False
  txt <- optional (between (char '(') (char ')') stringLiteral)
  void (optional (char ';'))
  return (T.unpack tag, mods, txt, hasChildren)

------------------------------------------------------------
------------------------------------------------------------
-- ネスト解析
------------------------------------------------------------

parseElement :: Maybe Pos -> Parser Node
parseElement parentIndent =
  case parentIndent of
    Nothing  -> L.nonIndented scn parseBody
    Just ind -> L.indentGuard scn GT ind *> parseBody
  where
    parseBody = do
      pos <- L.indentLevel
      (tag, mods, txt, hasChildren) <- tagLine
      children <-
        if hasChildren
          then many (try (tryDeeper pos *> parseElement (Just pos)))
          else pure []
      let inner =
            maybe children (\s -> Text s : children) txt
      pure (Element tag mods [] inner)

    tryDeeper :: Pos -> Parser ()
    tryDeeper parent =
      lookAhead $ do
        scn
        lvl <- L.indentLevel
        if lvl > parent
          then pure ()
          else empty

------------------------------------------------------------
-- トップレベル
------------------------------------------------------------

parseAster :: Text -> Either (ParseErrorBundle Text Void) [Node]
parseAster input =
  runParser parser "aster" input
  where
    parser =
      scn *> many (notFollowedBy (scn *> eof) *> parseElement Nothing)
          <* scn <* eof
