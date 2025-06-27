# Unisonプログラムの配布と実行

## 1. 実行可能ファイルの作成

### UCMでのコンパイル

```ucm
.> compile myProgram myapp
```

これにより、ネイティブの実行可能ファイル `myapp` が生成されます。

### 例：Hello Worldプログラム

```unison
-- hello.u
main : '{IO, Exception} ()
main = do
  printLine "Hello, World!"
```

```ucm
.> add
.> compile main hello
```

実行：
```bash
./hello
```

## 2. Unisonプログラムの構造

### エントリーポイント

```unison
-- Unisonのmain関数は必ず以下のシグネチャを持つ
main : '{IO, Exception} ()
main = do
  -- プログラムのロジック
  printLine "Starting application..."
  -- コマンドライン引数の処理
  args = getArgs
  match args with
    [] -> printLine "No arguments provided"
    xs -> printLine ("Arguments: " ++ Text.join " " xs)
```

## 3. 依存関係の管理

### プロジェクト構造

```
myproject/
├── .unison/          # Unisonコードベース
├── scratch.u         # 開発用ファイル
├── main.u           # メインプログラム
└── lib/             # 依存ライブラリ
```

### 依存関係のバンドル

```ucm
-- 必要なライブラリをインストール
.> lib.install @unison/base
.> lib.install @unison/http

-- プロジェクトを作成
.> project.create myapp
myapp/main> load main.u
myapp/main> add
myapp/main> compile main myapp
```

## 4. 配布方法

### 方法1：実行可能ファイルの直接配布

```bash
# Linux/macOS
chmod +x myapp
./myapp

# 配布
tar -czf myapp.tar.gz myapp
```

### 方法2：Unison Shareでの配布

```ucm
-- コードをUnison Shareにプッシュ
myapp/main> push @username/myapp

-- 利用者側でインストール
.> pull @username/myapp/releases/1.0.0 myapp
.> switch myapp/main
myapp/main> compile main myapp
```

### 方法3：スクリプトとして実行

```unison
-- script.u
use io2

main : '{IO, Exception} ()
main = do
  printLine "This is a Unison script"
  -- スクリプトのロジック
```

```bash
# ucm run コマンドで直接実行
ucm run script.u
```

## 5. プラットフォーム別の配布

### クロスコンパイル

現在、Unisonは実行環境でのコンパイルが必要です。各プラットフォーム用のバイナリを作成：

```bash
# Linux
ucm compile main myapp-linux

# macOS
ucm compile main myapp-macos

# Windows
ucm compile main myapp.exe
```

### Dockerでの配布

```dockerfile
FROM ubuntu:latest

# Unisonのインストール
RUN apt-get update && apt-get install -y curl
RUN curl -L https://github.com/unisonweb/unison/releases/latest/download/ucm-linux.tar.gz | tar -xz
RUN mv ucm /usr/local/bin/

# アプリケーションのコピー
COPY .unison /app/.unison
WORKDIR /app

# コンパイル
RUN ucm compile main app

ENTRYPOINT ["./app"]
```

## 6. Webアプリケーションとしての配布

### HTTPサーバーの例

```unison
use http.{HttpRequest, HttpResponse}

handleRequest : HttpRequest -> HttpResponse
handleRequest req = 
  match req.path with
    "/" -> HttpResponse.ok "text/html" "<h1>Welcome to Unison Web App</h1>"
    "/api/hello" -> HttpResponse.json "{\"message\": \"Hello from Unison\"}"
    _ -> HttpResponse.notFound "Page not found"

main : '{IO, Exception} ()
main = do
  port = 8080
  printLine ("Starting server on port " ++ Nat.toText port)
  http.serve port handleRequest
```

### デプロイメント

```bash
# サーバーでコンパイル
ucm compile main webapp

# systemdサービスとして登録
sudo tee /etc/systemd/system/unison-webapp.service << EOF
[Unit]
Description=Unison Web Application
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/opt/webapp
ExecStart=/opt/webapp/webapp
Restart=always

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl enable unison-webapp
sudo systemctl start unison-webapp
```

## 7. ライブラリとしての配布

### ライブラリの公開

```ucm
-- ライブラリプロジェクトの作成
.> project.create @username/mylib
@username/mylib/main> load mylib.u
@username/mylib/main> add

-- テストの追加
@username/mylib/main> load tests.u
@username/mylib/main> add

-- ドキュメントの追加
@username/mylib/main> load README.u
@username/mylib/main> add

-- リリース
@username/mylib/main> push
```

### ライブラリの利用

```ucm
-- 他のプロジェクトで利用
.> lib.install @username/mylib/releases/1.0.0
```

## 8. パッケージマネージャーでの配布

### Homebrewでの配布（macOS/Linux）

```ruby
# Formula/unison-myapp.rb
class UnisonMyapp < Formula
  desc "My Unison Application"
  homepage "https://github.com/username/myapp"
  url "https://github.com/username/myapp/releases/download/v1.0.0/myapp-macos.tar.gz"
  sha256 "..."
  
  def install
    bin.install "myapp"
  end
end
```

### Nixでの配布

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "unison-myapp";
  src = ./myapp;
  
  installPhase = ''
    mkdir -p $out/bin
    cp myapp $out/bin/
  '';
}
```

## 9. CI/CDパイプライン

### GitHub Actions例

```yaml
name: Build and Release

on:
  push:
    tags:
      - 'v*'

jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
    runs-on: ${{ matrix.os }}
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Install Unison
      run: |
        curl -L https://github.com/unisonweb/unison/releases/latest/download/ucm-${{ runner.os }}.tar.gz | tar -xz
        echo "$PWD" >> $GITHUB_PATH
    
    - name: Compile
      run: |
        ucm compile main myapp-${{ runner.os }}
    
    - name: Upload artifact
      uses: actions/upload-artifact@v3
      with:
        name: myapp-${{ runner.os }}
        path: myapp-${{ runner.os }}
```

## まとめ

Unisonは以下の方法で配布できます：

1. **実行可能ファイル**: `compile`コマンドでネイティブバイナリを生成
2. **Unison Share**: コードを共有し、利用者がローカルでコンパイル
3. **Docker**: コンテナイメージとして配布
4. **パッケージマネージャー**: Homebrew、Nix等での配布
5. **Webアプリ**: HTTPサーバーとしてデプロイ

選択する方法は、アプリケーションの性質とターゲットユーザーによって決まります。