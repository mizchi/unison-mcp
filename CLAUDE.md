# Unison MCP Server - 使用ガイド

## 概要
このプロジェクトはUnison言語のMCP (Model Context Protocol) サーバー実装です。
Unison Code Manager (UCM) をMCP経由で操作できます。

## Unison Shareからライブラリをインストールする方法

### 1. ライブラリ名が分かっている場合
```
mcp__unison__ucm_lib_install で @owner/library を指定
例: @unison/base, @unison/http, @hojberg/nanoid
```

### 2. インストール後の確認方法
- `mcp__unison__ucm_ls` で "lib" を指定して、インストール済みライブラリ一覧を表示
- 命名規則: `@owner/library` は `lib.owner_library_version` として配置される
  - 例: `@hojberg/nanoid` → `lib.hojberg_nanoid_1_0_0`

### 3. ライブラリを探す場合
- `mcp__unison__ucm_share_search` は実際の検索機能を持たないため、以下の方法を使用：
  1. https://share.unison-lang.org/ をブラウザで直接確認
  2. 一般的なライブラリ名で推測してインストール試行
  3. インストール後に `ls lib` で確認

### 4. 注意事項
- `find` コマンドの結果が大きすぎる場合、エラーになることがある
  - その場合は名前空間を指定して検索範囲を絞る
- インストール直後は出力がない場合があるが、`ls lib` で確認可能

## よく使うライブラリ

### @unison/base
- 基本的な型と関数（Text, List, Maybe, Either など）
- UUID生成、文字列操作、コレクション操作

### @unison/http
- HTTPクライアント/サーバー機能
- RESTful APIの構築

### @hojberg/nanoid
- URL安全な一意ID生成器
- カスタマイズ可能なアルファベットと長さ

### @unison/distributed
- 分散コンピューティング用ユーティリティ

### @unison/cli
- コマンドラインインターフェース構築用ツール

## トラブルシューティング

### ライブラリが見つからない場合
1. `ls lib` でインストール済みライブラリを確認
2. 名前空間の命名規則を確認（アンダースコア変換）
3. バージョン番号が付加されている可能性を確認

### 大きな検索結果でエラーになる場合
- 名前空間を指定して検索
- 例: `lib.hojberg_nanoid_1_0_0` 内のみを検索