# Aster

「Aster」は、独自記法で HTML を生成するための試作中コンパイラです。  
まだ製作途中で、仕様やファイル構成は今後大きく変わる可能性があります。

## 現状の機能
- `div#main.red:` のようにタグ名＋`#id`＋`.class` を組み合わせて記述
- `:` とインデントでネストを表現し、子要素を入れ子にできる
- 文字列リテラル（`"text"`）や末尾のセミコロンに対応
- `stack exec aster-project` で `aster-files/index.aster` を読み、`index.html` を出力

## 必要環境
- GHC 9.10 / Stack 最新版
- `megaparsec`, `text`, `directory`, `filepath`（`stack build` で自動取得）

## 使い方（暫定）
1. `stack build`
2. `stack exec aster-project`
3. `aster-files/index.html` が生成されるのでブラウザ等で確認

> **Note**: Aster 言語の仕様は試行錯誤中です。ファイル形式や CLI インターフェースは予告なく変更されます。

## 今後やりたいこと
- 属性や複数引数、変数参照などの追加記法
- `Include`/`Block` 等 AST ノードの実装
- テスト整備とドキュメントの充実