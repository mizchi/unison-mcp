# Unison開発ワークフローとUCM使用ガイド

## UCM（Unison Codebase Manager）とは

UCM（Unison Codebase Manager）は、Unisonプログラミング言語を実行し、作成したUnisonコードとやり取りするためのコマンドラインツールです。UCMは、Unisonコードベースへのインターフェースとして機能します。

### なぜUCMが必要なのか

Unisonコードはテキストベースのファイル内容として保存されません。そのため、Unisonプログラムを変更・実行するための専用ツールが必要です。

## 基本的なワークフロー

### 1. コードベースの初期化

```bash
ucm
```

`ucm`を実行すると、`$HOME/.unison`にUnisonコードベースが初期化されます。ここに関数定義、型、名前空間などが保存されます。

### 2. プロジェクト構造

Unisonプロジェクトは、共有・協力・バージョン管理が可能なライブラリやアプリケーションを表すコードベースの概念です。

```ucm
scratch/main> project.create myproject
```

## 主要なUCMコマンド

### 基本コマンド

| コマンド | 説明 |
|---------|------|
| `add` | 最後に型チェックされたファイルからすべての定義をコードベースに追加 |
| `update` | 既存の定義を置き換えて、依存関係を自動的に更新 |
| `run` | IO能力を必要とする項を実行 |
| `switch` | プロジェクトやブランチを切り替え |
| `history` | 現在のブランチへの変更履歴を表示 |
| `find` | 名前や型シグネチャで定義を検索 |
| `ls` | 名前空間の内容を一覧表示 |

## 開発フロー

### 1. scratch.uファイルの作成

テキストエディタで`.u`拡張子のファイルを作成します。UCMは現在のディレクトリのすべての`.u`ファイルを監視します。

```unison
-- scratch.u
greet : Text -> Text
greet name = "Hello, " ++ name ++ "!"
```

### 2. Watch式の使用

`>`で始まる行はwatch式です。UCMが自動的に式を評価し、結果をコンソールに表示します。

```unison
> greet "World"
-- 結果: "Hello, World!"
```

### 3. コードの追加

ファイルを保存すると、UCMが自動的に型チェックを行います。問題がなければ、`add`または`update`でコードベースに追加します。

```ucm
scratch/main> add
```

## プロジェクトとブランチ

### プロジェクトの作成

```ucm
scratch/main> project.create myproject
```

新しいプロジェクトには自動的に`base`標準ライブラリが含まれます。

### プロジェクトの一覧と切り替え

```ucm
# プロジェクト一覧を表示
> projects

# プロジェクトを切り替え
> switch scratch
> switch myproject/main
```

### 依存関係の管理

`lib.install`コマンドで追加の依存関係をインストールできます：

```ucm
myproject/main> lib.install @unison/http
```

### ブランチの活用

Gitのブランチと同様に、独立した開発ワークフローを可能にします：

```ucm
myproject/main> branch.create feature/new-feature
myproject/feature/new-feature> switch main
```

## 重要な概念

### 名前空間

Unisonコードベースは「名前空間」で整理されます。ファイルシステムのディレクトリのように機能し、型と関数を含みます。

- パスはドット（`.`）で区切られます（例：`base.Text`）
- `ls`コマンドで名前空間の内容を確認できます

### コンテンツアドレス指定

Unisonは関数や型を、その内容のハッシュで識別します。これにより：
- 名前の変更が簡単
- 依存関係の自動追跡
- コードの重複排除

## 便利な使い方のヒント

### ヘルプの活用

```ucm
> help
> help add
```

### 型による検索

型シグネチャで関数を検索できます：

```ucm
> find : [a] -> [[a]]
```

### 特定の名前空間の探索

```ucm
> ls base.data.List
```

### テストの実行

```ucm
> test
> test mytest
```

## 実践的な例

### 新しい関数の追加

1. `scratch.u`に関数を記述：
```unison
factorial : Nat -> Nat
factorial n = match n with
  0 -> 1
  n -> n * factorial (n - 1)
```

2. UCMで確認：
```ucm
scratch/main> add
```

### 既存の関数の更新

1. 関数を修正：
```unison
factorial : Nat -> Nat
factorial n = match n with
  0 -> 1
  1 -> 1
  n -> n * factorial (n - 1)
```

2. 更新を適用：
```ucm
scratch/main> update
```

## まとめ

UCMは従来のファイルベースのシステムとは異なるコード管理アプローチを提供し、バージョン管理、コラボレーション、コード整理のための強力な機能を提供します。この独特なアプローチにより、より安全で効率的なコード管理が可能になります。