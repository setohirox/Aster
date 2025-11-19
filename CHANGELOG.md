# Changelog for `aster-project`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2025-11-19
### Added
- 最初の Stack プロジェクト雛形
- `Aster.Parser` による Aster 言語（タグ＋`.class`/`#id`、ネスト構文）の基本実装
- `Aster.Renderer` によるシンプルな HTML 出力機能
- `stack exec aster-project` で `aster-files/index.aster` から `index.html` を生成する CLI
