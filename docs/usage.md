# Unison MCP Server 使用ガイド

## セットアップ

### 1. ビルド
```bash
cd unison-mcp-server
stack build
```

### 2. MCPクライアント設定

Claude Desktop や他のMCPクライアントの設定ファイルに以下を追加：

```json
{
  "mcpServers": {
    "unison": {
      "command": "/path/to/unison-mcp-server",
      "args": ["/path/to/your/unison/codebase"]
    }
  }
}
```

## 利用可能なツール

### 1. `ucm_find` - 定義の検索
```json
{
  "tool": "ucm_find",
  "arguments": {
    "query": "factorial"
  }
}
```

### 2. `ucm_add` - コードの追加
```json
{
  "tool": "ucm_add",
  "arguments": {
    "code": "double : Nat -> Nat\ndouble x = x * 2"
  }
}
```

### 3. `ucm_run` - 式の実行
```json
{
  "tool": "ucm_run",
  "arguments": {
    "expression": "factorial 5"
  }
}
```

### 4. `ucm_view` - ソースコードの表示
```json
{
  "tool": "ucm_view",
  "arguments": {
    "name": "List.map"
  }
}
```

### 5. プロジェクト管理
- `ucm_list_projects` - プロジェクト一覧
- `ucm_switch_project` - プロジェクト切り替え
- `ucm_list_branches` - ブランチ一覧
- `ucm_switch_branch` - ブランチ切り替え

### 6. `ucm_dependencies` - 依存関係の表示
```json
{
  "tool": "ucm_dependencies",
  "arguments": {
    "name": "myFunction"
  }
}
```

## 使用例

### AIアシスタントとの対話例

```
User: Unisonでfibonacci関数を実装してください

AI: fibonacci関数を実装します。
[ucm_add ツールを使用]

User: fibonacci 10 を実行してみてください

AI: fibonacci 10 を実行します。
[ucm_run ツールを使用]
結果: 55
```

## トラブルシューティング

### UCMが見つからない
環境変数PATHにUCMが含まれていることを確認してください。

### コードベースエラー
指定したパスに有効なUnisonコードベースが存在することを確認してください。

### ポート競合
UCMが既に起動している場合は、終了してからMCPサーバーを起動してください。