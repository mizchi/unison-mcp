# UCM（Unison Codebase Manager）の使い方

## UCMとは

UCM（Unison Codebase Manager）は、Unisonのコードベースを管理するためのツールです。UCMは現在のディレクトリ内の`.u`拡張子を持つファイル（スクラッチファイル）の変更を監視し、保存時に自動的にパースと型チェックを行います。

## 初期設定

```bash
# UCMを起動
ucm

# これにより $HOME/.unison にコードベースが初期化されます
```

## 主要なUCMコマンド

### プロジェクト管理

#### プロジェクトの作成
```ucm
.> project.create myProject
```
- 新しいプロジェクトと`/main`ブランチを作成
- デフォルトで`lib`名前空間を作成し、標準ライブラリ`base`をインストール

#### プロジェクト一覧の表示
```ucm
.> projects
```

### ブランチ管理

```ucm
# 新しいブランチを作成
.> branch feature/new-feature

# すべてのブランチを表示
.> branches

# ブランチをマージ
.> merge feature/new-feature
```

### コード管理

#### コードの追加・更新
```ucm
# 最新の型チェック済みファイルから定義を追加
.> add

# 既存の定義を更新
.> update

# 特定の定義を削除
.> delete myFunction

# 名前空間とその内容を削除
.> delete.namespace myNamespace
```

### ナビゲーションと検索

```ucm
# 現在の名前空間の内容を表示
.> ls

# 特定の名前空間の内容を表示
.> ls myNamespace

# 名前や型で検索
.> find myFunction
.> find : Nat -> Text
```

### 履歴とバージョン管理

```ucm
# ブランチの変更履歴を表示
.> reflog

# プロジェクト全体の変更履歴を表示
.> project.reflog

# 特定の状態にリセット
.> reset #abc123
```

### リモート操作

#### プロジェクトのクローン
```ucm
.> clone @username/project-name
```

#### プロジェクトのプッシュ
```ucm
# 現在のプロジェクトをプッシュ
.> push @username/project-name

# 特定の名前空間をプッシュ
.> push @username/project-name myNamespace
```

#### 依存関係のインストール
```ucm
.> lib.install @username/library-name
```

### その他の便利なコマンド

```ucm
# 名前の変更（リネーム）
.> move oldName newName

# ヘルプの表示
.> help
.> help add

# UCMを終了
.> quit
```

## プロジェクト構造

Unisonのプロジェクトは以下の構造を持ちます：

```
myProject/
├── main/           # メインブランチ
│   ├── lib/        # 依存関係
│   └── ...         # プロジェクトのコード
└── feature/        # フィーチャーブランチ
    └── ...
```

## ワークフロー例

### 1. 新しい関数の追加

```unison
-- myFile.u
add : Nat -> Nat -> Nat
add x y = x + y

-- ウォッチ式でテスト
> add 2 3
```

UCMで：
```ucm
.> add
```

### 2. 既存の関数の更新

```unison
-- myFile.u を編集
add : Nat -> Nat -> Nat
add x y = x + y + 1  -- 変更
```

UCMで：
```ucm
.> update
```

### 3. ローカルUIの使用

```ucm
.> ui
```
これによりブラウザでローカルコードベースのUIが開きます。

## ウォッチ式

`.u`ファイルで`>`で始まる行はウォッチ式として扱われます：

```unison
-- calculations.u
double : Nat -> Nat
double x = x * 2

> double 5
> List.map double [1, 2, 3, 4, 5]
```

UCMはこれらの式を自動的に評価し、結果を表示します。

## 名前空間

Unisonでは、名前空間は名前と定義のマッピングです。ファイルシステムのディレクトリ構造のように考えることができます：

```ucm
# 名前空間の作成と移動
.> cd myapp.utils

# 現在の場所を確認
.> pwd
```

## プロジェクトの依存関係

プロジェクトの依存関係は、プロジェクトのルートにある`lib`名前空間に配置されます：

```ucm
# 依存関係のインストール
.> lib.install @unison/base

# インストールされた依存関係の確認
.> ls lib
```

## 注意点

- UCMのコマンドはプロジェクトとブランチにスコープされています（v0.5.25以降）
- コードは従来のファイルではなく、コンテンツハッシュで保存されます
- これにより強力なバージョン管理と依存関係管理が可能になります

UCMは、Unisonの革新的なコード管理アプローチの中核を成すツールであり、コンテンツアドレス型のコード管理により、従来の開発ワークフローの多くの問題を解決します。