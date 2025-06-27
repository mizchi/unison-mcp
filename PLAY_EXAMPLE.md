# Unison MCP Server 実装例

このファイルには、Unison MCP Serverを使って実装した機能の例が含まれています。

## HTTPクライアント実装

Unisonの標準ライブラリには高レベルのHTTPクライアントが含まれていないため、低レベルのTCP/TLS接続を使ってHTTPクライアントを実装しました。

### 実装した関数

#### 1. 基本的なHTTPクライアント関数

**simpleHttpGet : Text -> Text -> Nat -> {IO, Exception} Text**
```unison
simpleHttpGet host path port =
  use Text ++
  conn = Connection.client (HostName.HostName host) (Port.number port)
  
  request = 
    "GET " ++ path ++ " HTTP/1.1\r\n" ++
    "Host: " ++ host ++ "\r\n" ++
    "User-Agent: Unison-HTTP-Client/1.0\r\n" ++
    "Accept: */*\r\n" ++
    "Connection: close\r\n" ++
    "\r\n"
  
  Connection.send conn (Text.toUtf8 request)
  responseBytes = Connection.receive conn
  Connection.close conn
  Text.fromUtf8 responseBytes
```

**simpleHttpsGet : Text -> Text -> {IO, Exception} Text**
```unison
simpleHttpsGet host path =
  -- TLS接続を使用（ポート443）
  conn = Connection.tls (HostName.HostName host) (Port.number 443)
  -- 以下はsimpleHttpGetと同じ処理
```

#### 2. HTTPレスポンス処理関数

**splitHttpResponse : Text -> (Text, Text)**
- HTTPレスポンスをヘッダーとボディに分離
- "\r\n\r\n"を区切りとして使用

**getStatusCode : Text -> Optional Nat**
- HTTPレスポンスからステータスコードを抽出
- "HTTP/1.1 200 OK"のような形式をパース

**getStatusCodeFromLine : Text -> Optional Nat**
- ステータスラインから数値部分を抽出するヘルパー関数

#### 3. 高度なHTTP機能

**makeHttpRequest : Text -> Text -> Text -> [(Text, Text)] -> Optional Text -> Text**
- カスタムヘッダーとボディを含むHTTPリクエストを構築
- メソッド、パス、ホスト、ヘッダー、ボディを指定可能

**httpGetWithHeaders : Text -> Text -> Nat -> [(Text, Text)] -> {IO, Exception} Text**
- カスタムヘッダー付きのGETリクエストを送信
- 認証ヘッダーやカスタムUser-Agentなどを設定可能

### 使用例

#### 例1: シンプルなHTTP GET
```unison
response = simpleHttpGet "httpbin.org" "/get" 80
printLine response
```

#### 例2: HTTPS GET（GitHub API）
```unison
response = simpleHttpsGet "api.github.com" "/users/github"
printLine response
```

#### 例3: カスタムヘッダー付きリクエスト
```unison
headers = [
  ("User-Agent", "Unison-Custom-Client/2.0"),
  ("Accept", "application/json"),
  ("Authorization", "Bearer YOUR_TOKEN_HERE")
]
response = httpGetWithHeaders "api.example.com" "/data" 443 headers
```

#### 例4: レスポンスの解析
```unison
sampleResponse = "HTTP/1.1 404 Not Found\r\nContent-Type: application/json\r\n\r\n{\"error\": \"Not Found\"}"

(headers, body) = splitHttpResponse sampleResponse

match getStatusCode sampleResponse with
  Some code -> printLine ("Status: " ++ Nat.toText code)  -- "Status: 404"
  None -> printLine "No status code found"
```

#### 例5: カスタムHTTPリクエストの作成
```unison
headers = [
  ("Content-Type", "application/json"),
  ("X-Custom-Header", "custom-value")
]

jsonBody = "{\"name\": \"Unison User\", \"email\": \"user@example.com\"}"

request = makeHttpRequest "POST" "/api/users" "api.example.com" headers (Some jsonBody)
```

### デモ関数

**demonstrateHttpParsing : '{IO, Exception} ()**
- HTTPレスポンスのパース機能をデモンストレーション
- サンプルレスポンスを使用してヘッダー/ボディの分離とステータスコード抽出を実演

**runAllHttpExamples : '{IO, Exception} ()**
- 全ての使用例を順番に実行
- HTTPクライアント機能の包括的なデモンストレーション

### 実装の詳細

以下は、実際にUnisonで実装した全関数のソースコードです：

```unison
-- 基本的なHTTP GET
simpleHttpGet : Text -> Text -> Nat -> {IO, Exception} Text
simpleHttpGet host path port =
  use Text ++
  conn = Connection.client (HostName host) (Port.number port)
  request =
    "GET "
      ++ path
      ++ " HTTP/1.1\r\n"
      ++ "Host: "
      ++ host
      ++ "\r\n"
      ++ "User-Agent: Unison-HTTP-Client/1.0\r\n"
      ++ "Accept: */*\r\n"
      ++ "Connection: close\r\n"
      ++ "\r\n"
  Connection.send conn (Text.toUtf8 request)
  responseBytes = Connection.receive conn
  Connection.close conn
  fromUtf8 responseBytes

-- HTTPS GET
simpleHttpsGet : Text -> Text -> {IO, Exception} Text
simpleHttpsGet host path =
  use Text ++
  conn = Connection.tls (HostName host) (Port.number 443)
  request =
    "GET "
      ++ path
      ++ " HTTP/1.1\r\n"
      ++ "Host: "
      ++ host
      ++ "\r\n"
      ++ "User-Agent: Unison-HTTP-Client/1.0\r\n"
      ++ "Accept: */*\r\n"
      ++ "Connection: close\r\n"
      ++ "\r\n"
  Connection.send conn (Text.toUtf8 request)
  responseBytes = Connection.receive conn
  Connection.close conn
  fromUtf8 responseBytes

-- レスポンスの分離
splitHttpResponse : Text -> (Text, Text)
splitHttpResponse response = match Text.indexOf "\r\n\r\n" response with
  None   -> (response, "")
  Some i ->
    use Nat +
    headers = Text.take i response
    body = Text.drop (i + 4) response
    (headers, body)

-- ステータスコード抽出
getStatusCode : Text -> Optional Nat
getStatusCode response =
  lines = Text.lines response
  List.head lines |> Optional.flatMap getStatusCodeFromLine

getStatusCodeFromLine : Text -> Optional Nat
getStatusCodeFromLine line = match Text.indexOf " " line with
  None    -> None
  Some i1 ->
    use Nat + fromText
    rest = Text.drop (i1 + 1) line
    match Text.indexOf " " rest with
      None    -> fromText rest
      Some i2 ->
        code = Text.take i2 rest
        fromText code

-- カスタムリクエスト作成
makeHttpRequest : Text -> Text -> Text -> [(Text, Text)] -> Optional Text -> Text
makeHttpRequest method path host headers body =
  use Text ++
  headerLines =
    headers
      |> (List.map cases (name, value) -> name ++ ": " ++ value)
      |> Text.join "\r\n"
  contentLength =
    match body with
      None -> ""
      Some content ->
        "Content-Length: " ++ Nat.toText (Text.size content) ++ "\r\n"
  bodyContent = match body with
    None         -> ""
    Some content -> content
  method
    ++ " "
    ++ path
    ++ " HTTP/1.1\r\n"
    ++ "Host: "
    ++ host
    ++ "\r\n"
    ++ headerLines
    ++ (if Text.isEmpty headerLines then "" else "\r\n")
    ++ contentLength
    ++ "Connection: close\r\n"
    ++ "\r\n"
    ++ bodyContent

-- カスタムヘッダー付きGET
httpGetWithHeaders : Text -> Text -> Nat -> [(Text, Text)] -> {IO, Exception} Text
httpGetWithHeaders host path port headers =
  conn = Connection.client (HostName host) (Port.number port)
  request = makeHttpRequest "GET" path host headers None
  Connection.send conn (Text.toUtf8 request)
  responseBytes = Connection.receive conn
  Connection.close conn
  fromUtf8 responseBytes
```

### 制限事項と今後の改善点

1. **エラーハンドリング**: 現在の実装は基本的なもので、ネットワークエラーやタイムアウトの処理が不十分
2. **レスポンスパーシング**: HTTPレスポンスのパースは簡易的で、チャンク転送エンコーディングやgzip圧縮には対応していない
3. **リダイレクト**: HTTPリダイレクト（3xx）の自動追跡は実装されていない
4. **接続プーリング**: 毎回新しい接続を作成するため、パフォーマンスが最適ではない
5. **証明書検証**: HTTPS接続の証明書検証の詳細な制御ができない

### まとめ

この実装は、Unisonで基本的なHTTPリクエストを送信するための最小限の機能を提供します。本格的な使用には、より高度なHTTPクライアントライブラリの開発またはFFIを通じた外部ライブラリの利用が推奨されます。

## その他の実装例

### 基本的な関数定義

最初のMCPセッションでは、以下の基本的な関数も実装しました：

- `id : a -> a` - 恒等関数
- `head : [a] -> Optional a` - リストの最初の要素を取得
- `pair : a -> b -> (a, b)` - 2つの値のペアを作成
- `compose : (b -> c) -> (a -> b) -> (a -> c)` - 関数合成
- `length : [a] -> Nat` - リストの長さを計算
- `reverse : [a] -> [a]` - リストを逆順にする
- `map : (a -> b) -> [a] -> [b]` - リストの各要素に関数を適用
- `filter : (a -> Boolean) -> [a] -> [a]` - 条件に合う要素のみを抽出

これらの関数は、UnisonのMCPサーバーを通じて正常に定義され、型安全性が保証されています。

## Unison Shareからライブラリをインストールする方法

### 1. インストール方法

Unison MCPサーバーには`mcp__unison__ucm_lib_install`ツールが用意されており、Unison Shareからライブラリを簡単にインストールできます。

```
# 使用例
mcp__unison__ucm_lib_install
  library: "@unison/http"
```

### 2. 実際にインストールした例：公式HTTPライブラリ

今回、`@unison/http`ライブラリをインストールしました。このライブラリは我々が独自実装した簡易HTTPクライアントよりも多くの機能を提供します。

#### インストール結果
- インストール前：`lib/`には8,013 terms, 184 types
- インストール後：`lib/`には31,958 terms, 824 types
- 追加されたパス：`lib.unison_http_3_9_1/`

### 3. 公式HTTPライブラリの使用例

```unison
-- 簡単なGETリクエスト
httpGetOfficial : Text -> '{IO, Exception} lib.unison_http_3_9_1.HttpResponse
httpGetOfficial url = do
  use lib.unison_http_3_9_1.client.Http
  Http.run do
    Http.get (lib.base.IO.net.URI.parseOrBug url)

-- レスポンスボディをテキストとして取得
getBodyText : lib.unison_http_3_9_1.HttpResponse -> Text
getBodyText response =
  use lib.unison_http_3_9_1
  body = HttpResponse.body response
  bytes = Body.toBytes body
  lib.base.Text.fromUtf8 bytes

-- HTTPレスポンスの詳細を表示
showHttpResponse : lib.unison_http_3_9_1.HttpResponse -> Text
showHttpResponse response =
  use lib.unison_http_3_9_1
  use lib.base.Text ++
  use lib.base.Nat toText
  
  statusCode = HttpResponse.statusCode response |> HttpResponse.Status.code |> toText
  statusText = HttpResponse.statusCode response |> HttpResponse.Status.text
  
  headerCount = HttpResponse.headers response |> Headers.toList |> lib.base.data.List.size |> toText
  
  bodyBytes = HttpResponse.body response |> Body.toBytes
  bodySize = lib.base.data.Bytes.size bodyBytes |> toText
  
  "Status: " ++ statusCode ++ " " ++ statusText ++ "\n" ++
  "Headers: " ++ headerCount ++ " headers\n" ++
  "Body: " ++ bodySize ++ " bytes"
```

### 4. 公式ライブラリで追加される機能

公式HTTPライブラリ（`@unison/http`）は以下の高度な機能を提供します：

- **HTTPクライアント** (`lib.unison_http_3_9_1.client`)
  - GET、POST、PUT、DELETE等の全HTTPメソッド
  - カスタムヘッダー、ボディの完全なサポート
  - プロキシ設定
  - 詳細なエラーハンドリング

- **HTTPサーバー** (`lib.unison_http_3_9_1.server`)
  - HTTPサーバーの作成
  - ルーティング
  - ミドルウェアサポート

- **WebSocket** (`lib.unison_http_3_9_1.websockets`)
  - WebSocketクライアント/サーバー
  - 双方向通信

### 5. ライブラリの探索方法

インストール後、以下のコマンドでライブラリの内容を探索できます：

```
# ライブラリ一覧
mcp__unison__ucm_ls
  namespace: "lib"

# 特定のライブラリの内容
mcp__unison__ucm_ls  
  namespace: "lib.unison_http_3_9_1"

# 関数の詳細を確認
mcp__unison__ucm_view
  name: "lib.unison_http_3_9_1.client.Http.get"
```

### 6. その他の利用可能なライブラリ

Unison Shareには多くのライブラリが公開されています。主なものは：

- `@unison/base` - 基本ライブラリ（通常はデフォルトでインストール済み）
- `@unison/http` - HTTP/WebSocketライブラリ
- その他のライブラリは https://share.unison-lang.org/ で検索可能

### まとめ

Unison MCPサーバーの`mcp__unison__ucm_lib_install`ツールを使用することで、Unison Shareから公開されているライブラリを簡単にインストールして使用できます。今回インストールした公式HTTPライブラリは、我々が実装した簡易版と比較して、より完全で本番環境でも使用可能な機能を提供しています。

## 新機能：Unison Share検索とインストール

### 実装したMCPコマンド

#### 1. `mcp__unison__ucm_share_search` - Unison Shareでライブラリを検索

```
# 使用例
mcp__unison__ucm_share_search
  query: "json"
```

このコマンドは、Unison Shareでライブラリを検索しようとします。ただし、現在のUCMには直接的なShare検索コマンドがないため、代替として以下の情報を提供します：
- Unison Shareのウェブサイト（https://share.unison-lang.org/）へのリンク
- よく使われるライブラリのリスト
- インストール方法の説明

#### 2. `mcp__unison__ucm_share_install` - 特定バージョンのライブラリをインストール

```
# 使用例1：デフォルト名でインストール
mcp__unison__ucm_share_install
  library: "@unison/http/releases/3.9.1"

# 使用例2：カスタム名でインストール
mcp__unison__ucm_share_install
  library: "@unison/http/releases/3.9.1"
  as: "http_v3"
```

このコマンドは、Unison Shareから特定のライブラリをインストールします。`as`パラメータを使用することで、ライブラリのローカル名を指定できます。

### 実装の詳細

#### Tools.hs への追加

```haskell
-- 新しいツール定義
Tool
  { toolName = "ucm_share_search"
  , toolDescription = Just "Search for libraries on Unison Share"
  , toolInputSchema = object [...]
  }

Tool
  { toolName = "ucm_share_install"
  , toolDescription = Just "Install a library from Unison Share with full path"
  , toolInputSchema = object [...]
  }

-- ハンドラー関数
handleShareSearch :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleShareSearch ucm (Just params) = do
  -- 検索クエリを取得してUCMコマンドを実行

handleShareInstall :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleShareInstall ucm (Just params) = do
  -- ライブラリパスとオプション名を取得してインストール
```

#### UCM.hs への追加

```haskell
-- Unison Share検索機能
searchShare :: UCMHandle -> Text -> IO Text
searchShare handle query = do
  -- find.global コマンドを試行
  -- 失敗した場合は代替情報を提供

-- 特定バージョンのライブラリをインストール
installFromShare :: UCMHandle -> Text -> Maybe Text -> IO Text
installFromShare handle libraryPath asName = do
  -- lib.install コマンドにオプション名を追加して実行
```

### 使用シナリオ

1. **ライブラリの探索**
   ```
   User: JSONライブラリを探してください
   AI: mcp__unison__ucm_share_searchでjsonを検索します
   ```

2. **特定バージョンのインストール**
   ```
   User: @unison/httpの最新版をインストールしてください
   AI: mcp__unison__ucm_share_installで@unison/http/releases/3.9.1をインストールします
   ```

3. **複数バージョンの管理**
   ```
   User: httpライブラリの古いバージョンも保持したい
   AI: mcp__unison__ucm_share_installでasパラメータを使い、http_oldという名前でインストールします
   ```

### 制限事項

- UCMには現在、Unison Shareを直接検索するコマンドがないため、検索機能は限定的
- ライブラリのバージョン一覧を取得する機能は未実装
- インストール済みライブラリの更新チェック機能は未実装

これらの新機能により、Unison Shareのライブラリをより柔軟に管理できるようになりました。