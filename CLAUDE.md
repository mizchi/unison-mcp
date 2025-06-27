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

## 汎用コマンド実行

`mcp__unison__ucm_command`を使うと、個別のMCPツールが用意されていないUCMコマンドも実行できます。

### 重要な注意事項
- これは低レベルツールです - 使用前にコマンド構文と型要件を確認してください
- UCMからのエラーはそのまま返されます
- 引数は`args`配列として個別に渡します

### 使用例

```
# 引数なしのコマンド
mcp__unison__ucm_command で {"command": "help"} を実行
mcp__unison__ucm_command で {"command": "reflog"} を実行

# 引数付きのコマンド
mcp__unison__ucm_command で {"command": "pull", "args": ["@unison/base/main"]} を実行
mcp__unison__ucm_command で {"command": "push", "args": ["myproject.public"]} を実行
mcp__unison__ucm_command で {"command": "fork", "args": [".base", "lib.mybase"]} を実行
mcp__unison__ucm_command で {"command": "view", "args": ["factorial"]} を実行
mcp__unison__ucm_command で {"command": "diff.namespace", "args": ["1", "7"]} を実行
```

この汎用コマンドにより、UCMの全機能にアクセスできます。

## テスト実行

`mcp__unison__ucm_test`を使うと、簡単にテストを実行できます：

### 特徴
- scratch.uが存在しない場合、サンプルテストを含むファイルを自動生成
- scratch.uが存在する場合、そのファイルを読み込んでテストを実行
- パターンを指定して特定のテストのみ実行可能

### 使用例
```
# すべてのテストを実行（scratch.uを自動生成または読み込み）
mcp__unison__ucm_test

# 特定のパターンにマッチするテストのみ実行
mcp__unison__ucm_test で {"pattern": "test.add"}
```

## scratch.uファイルの表示

`mcp__unison__ucm_view_scratch`を使うと、scratch.uファイルの内容を表示できます（UCMにloadせずに）：

### 使用例
```
# scratch.uの内容を表示
mcp__unison__ucm_view_scratch
```

これは現在のディレクトリにあるscratch.uファイルの内容を単に表示するだけで、UCMにロードはしません。

## トラブルシューティング

### ライブラリが見つからない場合
1. `ls lib` でインストール済みライブラリを確認
2. 名前空間の命名規則を確認（アンダースコア変換）
3. バージョン番号が付加されている可能性を確認

### 大きな検索結果でエラーになる場合
- 名前空間を指定して検索
- 例: `lib.hojberg_nanoid_1_0_0` 内のみを検索