# Unisonの基本構文と型システム

## 基本的な値と関数

### 関数定義

Unisonでは、関数定義はインデントまたは`let`ブロックを使用して記述します：

```unison
-- 基本的な関数定義
repeatNum : Nat -> Text
repeatNum num = 
  text = Nat.toText num
  Text.repeat num text
```

### 型アノテーション

型アノテーションは `e : T` の形式で記述します。ここで `e` は式、`T` は型を表します：

```unison
-- 単純な型アノテーション
inc : Nat -> Nat
inc x = x + 1

-- 多相型関数
id : ∀ t. t -> t
id x = x
```

### 関数型

`X -> Y` は、型 `X` の引数を受け取り、型 `Y` の結果を返す関数の型です。`->` は右結合なので、`X -> Y -> Z` は `X -> (Y -> Z)` と同じです。

## ラムダ式（無名関数）

ラムダ式は `->` 記法を使用します：

```unison
-- 単純なラムダ
elem -> elem + 1

-- 複数引数のラムダ
List.foldLeft (acc a -> a Nat.+ acc) 0 [1, 2, 3, 4]

-- パターンマッチングを使用したラムダ
List.map (cases (a, b) -> a Nat.+ b) [(1, 2), (3, 4)]
```

## 型システムの特徴

### 1. 基本型

Unisonには以下の組み込み型があります：

- `Nat` - 自然数
- `Int` - 整数
- `Float` - 浮動小数点数
- `Text` - 文字列
- `Boolean` - 真偽値
- `Bytes` - バイト列

### 2. タプル型

```unison
-- ペア（2要素タプル）
pair : (Nat, Text)
pair = (42, "hello")

-- トリプル（3要素タプル）
triple : (Nat, Text, Boolean)
triple = (1, "world", true)
```

### 3. リスト型

```unison
-- 数値のリスト
numbers : [Nat]
numbers = [1, 2, 3, 4, 5]

-- 文字列のリスト
words : [Text]
words = ["hello", "world"]
```

### 4. オプション型

```unison
-- Optional型の定義
structural type Optional a
  = Some a
  | None

-- 使用例
maybeNumber : Optional Nat
maybeNumber = Some 42
```

## データ型の定義

### 構造型（Structural Type）

```unison
structural type Color
  = Red
  | Green
  | Blue
  | RGB Nat Nat Nat

-- パターンマッチング
colorToText : Color -> Text
colorToText = cases
  Red -> "red"
  Green -> "green"
  Blue -> "blue"
  RGB r g b -> "rgb(" ++ Nat.toText r ++ "," ++ Nat.toText g ++ "," ++ Nat.toText b ++ ")"
```

### ユニーク型（Unique Type）

```unison
unique type UserId = UserId Nat

-- ユニーク型は構造が同じでも異なる型として扱われる
unique type ProductId = ProductId Nat

-- UserIdとProductIdは混同されない
```

## パターンマッチング

`match` 式または `cases` を使用してパターンマッチングを行います：

```unison
-- match式
describe : Optional Nat -> Text
describe opt = match opt with
  Some n -> "The number is " ++ Nat.toText n
  None -> "No number"

-- cases（より簡潔な記法）
describe2 : Optional Nat -> Text
describe2 = cases
  Some n -> "The number is " ++ Nat.toText n
  None -> "No number"
```

## 関数合成

パイプ演算子 `|>` を使用した関数合成：

```unison
-- パイプ演算子の使用
result : Text
result = 
  [1, 2, 3, 4, 5]
    |> List.map (x -> x * 2)
    |> List.filter (x -> x > 5)
    |> List.map Nat.toText
    |> Text.join ", "
-- 結果: "6, 8, 10"
```

## let式とwhere句

```unison
-- let式
calculate : Nat -> Nat
calculate n =
  let
    double = n * 2
    triple = n * 3
  in double + triple

-- where句（UCMでのみ使用可能）
calculate2 : Nat -> Nat
calculate2 n = double + triple
  where
    double = n * 2
    triple = n * 3
```

## エラー処理

Unisonでは、エラー処理は主に`Optional`型や`Either`型、そして能力（Abilities）を使用して行います：

```unison
-- Either型を使用したエラー処理
divide : Float -> Float -> Either Text Float
divide x y =
  if y == 0.0 then
    Left "Division by zero"
  else
    Right (x / y)
```

これらの基本構文を理解することで、Unisonでのプログラミングの基礎が身に付きます。