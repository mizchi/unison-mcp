# Unisonのインストールと環境構築

## システム要件

Unisonは以下のプラットフォームをサポートしています：

- Mac OS X（IntelおよびApple Silicon）
- 64ビットLinux
- Windows（Windows Terminalの使用を推奨）

## インストール方法

### 1. Homebrew（Mac）

最も簡単な方法：

```bash
brew tap unisonweb/unison
brew install unison-language
```

### 2. Debian/Ubuntu

#### 安定版リリース

```bash
# GPGキーの追加
curl https://debian.unison-lang.org/public.gpg | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/unison-computing.gpg

# リポジトリの追加
echo 'deb [arch=amd64 signed-by=/etc/apt/trusted.gpg.d/unison-computing.gpg] https://debian.unison-lang.org/ bookworm main' | sudo tee /etc/apt/sources.list.d/unison-computing.list

# パッケージのインストール
sudo apt update
sudo apt install unisonweb
```

### 3. 手動インストール

1. [GitHubリリースページ](https://github.com/unisonweb/unison/releases)から、使用しているプラットフォーム用のtarballをダウンロード
2. ファイルを展開
3. `ucm`実行ファイルを実行

### 4. Nixを使用したインストール

Nixパッケージマネージャーを使用する場合：
- https://github.com/ceedubs/unison-nix/ の手順に従ってください

## 初期セットアップ

### 1. UCM（Unison Codebase Manager）の起動

インストール後、ターミナルで以下を実行：

```bash
ucm
```

これにより、Unisonのコードベースが初期化されます。

### 2. プロジェクトの作成

UCMプロンプトで以下を実行：

```
.> project.create quickstart
```

これにより、`quickstart`という名前の新しいプロジェクトが作成されます。

### 3. エディタのセットアップ

開発効率を上げるため、以下のエディタ拡張機能のインストールを推奨：

#### VS Code
- Unison拡張機能をインストール
- シンタックスハイライト、コード補完、エラー表示などの機能が利用可能

## インストールの確認

インストールが正しく完了したか確認するには：

```bash
ucm version
```

バージョン情報が表示されれば、インストールは成功です。

## トラブルシューティング

### Windowsでの注意点
- Windows Terminalの使用を強く推奨
- パスに日本語が含まれる場合、問題が発生する可能性があります

### Linuxでの権限エラー
- `sudo`を使用してインストールコマンドを実行してください
- ユーザーのホームディレクトリに`.unison`ディレクトリが作成されることを確認

### Macでのセキュリティ警告
- システム環境設定でUnisonの実行を許可する必要がある場合があります