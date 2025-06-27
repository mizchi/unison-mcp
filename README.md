# unison-mcp-server

Unison言語のModel Context Protocol (MCP)サーバー実装です。AIアシスタントがUCM（Unison Codebase Manager）を操作できるようにします。

## 概要

このプロジェクトは、UnisonコードベースとやりとりするためのMCPインターフェースを提供し、AIアシスタントが以下の操作を実行できるようにします：

- 定義の検索
- コードの追加と更新
- Unison式の実行
- プロジェクトとブランチの管理
- 依存関係の分析

## 前提条件

### 1. Unisonのインストール

```bash
# macOS (Homebrew)
brew install unisonweb/unison/unison-language

# その他のOS
# https://www.unison-lang.org/install/ から最新版をダウンロード
```

### 2. Haskellのセットアップ

```bash
# Stack（Haskellビルドツール）のインストール
curl -sSL https://get.haskellstack.org/ | sh

# または
wget -qO- https://get.haskellstack.org/ | sh
```

## セットアップ

### 1. リポジトリのクローン

```bash
git clone https://github.com/yourusername/unison-mcp-server.git
cd unison-mcp-server
```

### 2. ビルド

```bash
stack build
```

### 3. Claude Desktop への登録

#### 手動設定

`~/Library/Application Support/Claude/claude_desktop_config.json` (macOS) または対応するパスに以下を追加：

```json
{
  "mcpServers": {
    "unison": {
      "command": "/path/to/unison-mcp-server/.stack-work/install/.../bin/unison-mcp-server-exe",
      "args": ["/path/to/your/unison/project"]
    }
  }
}
```

#### claude mcp add を使用（推奨）

```bash
# ビルドしたバイナリのパスを確認
stack path --local-install-root

# Claude MCP に追加
claude mcp add unison /path/to/binary/unison-mcp-server-exe /path/to/your/unison/project
```

## 使用方法

### 初期セットアップ

新しいUnisonプロジェクトを開始する場合：

```
# ターミナルで
cd /path/to/your/unison/project
ucm

# UCM内で
.myproject> project.create myproject
```

### Claude での使用例

Claude Desktopで以下のようなプロンプトを使用：

```
unison mcp を使ってコードを書いてみて
```

### 実行デモ

以下は実際の使用例です：

```unison
-- 基本的な関数定義
id : a -> a
id x = x

-- リスト操作
head : [a] -> Optional a
head = cases
  [] -> None
  x +: _ -> Some x

-- 高階関数
map : (a -> b) -> [a] -> [b]
map f = cases
  [] -> []
  x +: xs -> f x +: map f xs

-- フィルター関数
filter : (a -> Boolean) -> [a] -> [a]
filter p = cases
  [] -> []
  x +: xs -> if p x then x +: filter p xs else filter p xs
```

## アーキテクチャ

```
┌─────────────┐     JSON-RPC      ┌──────────────┐
│ AI Assistant│ ←───────────────→ │  MCP Server  │
└─────────────┘                   └──────┬───────┘
                                         │
                                         v
                                   ┌─────────────┐
                                   │     UCM     │
                                   └─────────────┘
```

## 利用可能なツール

MCPツールとして以下が利用可能です：

### 基本操作
- `mcp__unison__ucm_find` - 定義の検索
- `mcp__unison__ucm_add` - 新しい定義の追加
- `mcp__unison__ucm_run` - Unison式の実行
- `mcp__unison__ucm_view` - 定義のソースコード表示
- `mcp__unison__ucm_update` - 既存定義の更新
- `mcp__unison__ucm_ls` - 名前空間の内容一覧
- `mcp__unison__ucm_delete` - 定義の削除
- `mcp__unison__ucm_test` - テストの実行
- `mcp__unison__ucm_dependencies` - 定義の依存関係表示

### プロジェクト管理
- `mcp__unison__ucm_list_projects` - プロジェクト一覧
- `mcp__unison__ucm_switch_project` - プロジェクトの切り替え
- `mcp__unison__ucm_project_create` - 新規プロジェクト作成
- `mcp__unison__ucm_list_branches` - ブランチ一覧
- `mcp__unison__ucm_switch_branch` - ブランチの切り替え
- `mcp__unison__ucm_branch_create` - 新規ブランチ作成
- `mcp__unison__ucm_merge` - ブランチのマージ

### ライブラリ管理
- `mcp__unison__ucm_lib_install` - ライブラリのインストール（例: @unison/base）
- `mcp__unison__ucm_share_search` - Unison Shareのライブラリ情報を表示（実際の検索は未対応）
- `mcp__unison__ucm_share_install` - Unison Shareから特定バージョンをインストール
- インストール後: `mcp__unison__ucm_ls` で "lib" を指定して確認
- 命名規則: @owner/library → lib.owner_library_version

### ライブラリの使用例

```unison
-- baseライブラリをインストール後
use lib.unison_base_3_21_0

-- UUIDの生成
testUuid : '{IO, Exception} ()
testUuid = do
  uuid = Uuid.parse "550e8400-e29b-41d4-a716-446655440000"
  printLine (Uuid.toText uuid)

-- nanoidライブラリをインストール後
use lib.hojberg_nanoid_1_0_0
use lib.hojberg_nanoid_1_0_0.NanoId

-- NanoIDの生成
testNanoId : '{IO, Exception} ()
testNanoId = do
  id = nanoid()
  printLine (NanoId.toText id)
```

## トラブルシューティング

### UCMが見つからない場合

```bash
# PATHにUCMがあるか確認
which ucm

# なければPATHに追加
export PATH=$PATH:/path/to/unison/bin
```

### ビルドエラーの場合

```bash
# 依存関係を更新
stack update

# クリーンビルド
stack clean
stack build
```

### MCP接続エラーの場合

1. バイナリパスが正しいか確認
2. プロジェクトディレクトリが存在するか確認
3. UCMが正常に動作するか確認

## 開発

プロジェクト構造：

- `src/Unison/MCP/Server.hs` - MCPサーバーの実装
- `src/Unison/MCP/Protocol.hs` - MCPプロトコルの型定義
- `src/Unison/MCP/Tools.hs` - ツールの実装
- `src/Unison/MCP/UCM.hs` - UCM統合レイヤー

## ライセンス

このプロジェクトはMITライセンスで公開されています。
