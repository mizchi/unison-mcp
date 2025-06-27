# 実装計画詳細 - Unison MCP Server

## 即座に実装すべき機能（今週中）

### 1. エラーハンドリングの改善

#### 現在の問題点
- UCMプロセスがクラッシュしても検出できない
- タイムアウトが固定値（1秒）で調整不可
- エラーメッセージが不親切

#### 実装方針
```haskell
-- src/Unison/MCP/UCM.hs に追加
data UCMConfig = UCMConfig
  { configTimeout :: Int  -- ミリ秒
  , configRetryCount :: Int
  , configAutoRestart :: Bool
  }

-- プロセス監視機能
monitorProcess :: UCMHandle -> IO ()
monitorProcess handle = forever $ do
  exitCode <- getProcessExitCode (ucmProcess handle)
  case exitCode of
    Just code -> do
      -- 自動再起動ロジック
      restartUCM handle
    Nothing -> threadDelay 1000000  -- 1秒待機
```

### 2. 基本的なUCMコマンドの追加

#### pull/pushコマンド
```haskell
-- リモートから定義を取得
pullFromRemote :: UCMHandle -> Text -> IO Text
pullFromRemote handle remote = 
  sendCommand handle $ "pull " <> remote

-- リモートに定義をプッシュ
pushToRemote :: UCMHandle -> Text -> IO Text  
pushToRemote handle remote =
  sendCommand handle $ "push " <> remote
```

#### undo/redoサポート
```haskell
-- 直前の操作を取り消し
undoLastOperation :: UCMHandle -> IO Text
undoLastOperation handle =
  sendCommand handle "undo"

-- 取り消した操作をやり直し
redoOperation :: UCMHandle -> IO Text
redoOperation handle =
  sendCommand handle "redo"
```

## 次に実装すべき機能（来週）

### 3. Unison Share API統合

#### API調査タスク
1. Unison ShareのREST APIエンドポイント確認
2. 認証方式（OAuth? APIキー?）の調査
3. レート制限の確認

#### 実装イメージ
```haskell
-- src/Unison/MCP/Share.hs (新規ファイル)
module Unison.MCP.Share where

import Network.HTTP.Simple

data ShareClient = ShareClient
  { shareBaseUrl :: Text
  , shareApiKey :: Maybe Text
  }

-- ライブラリ検索
searchLibraries :: ShareClient -> Text -> IO [Library]
searchLibraries client query = do
  let request = parseRequest_ $ T.unpack $ 
        shareBaseUrl client <> "/api/search?q=" <> query
  response <- httpJSON request
  return $ getResponseBody response

-- ライブラリ詳細取得
getLibraryDetails :: ShareClient -> Text -> IO LibraryDetails
getLibraryDetails client libPath = do
  -- 実装
```

### 4. 依存関係の可視化

#### Graphvizを使った依存関係グラフ生成
```haskell
-- 依存関係をDOT形式で出力
exportDependencyGraph :: UCMHandle -> Text -> IO Text
exportDependencyGraph handle definition = do
  deps <- getDependencies handle definition
  return $ generateDotGraph definition deps

generateDotGraph :: Text -> [Text] -> Text
generateDotGraph root deps = 
  "digraph G {\n" <>
  "  " <> root <> ";\n" <>
  T.unlines (map (\d -> "  " <> root <> " -> " <> d <> ";") deps) <>
  "}"
```

## 中期的な改善（1ヶ月以内）

### 5. パフォーマンス最適化

#### コマンド結果のキャッシング
```haskell
-- src/Unison/MCP/Cache.hs
data CacheEntry = CacheEntry
  { cacheKey :: Text
  , cacheValue :: Text
  , cacheExpiry :: UTCTime
  }

type Cache = TVar (Map Text CacheEntry)

-- キャッシュ付きコマンド実行
cachedCommand :: Cache -> UCMHandle -> Text -> IO Text
cachedCommand cache handle cmd = do
  now <- getCurrentTime
  cached <- atomically $ do
    entries <- readTVar cache
    return $ M.lookup cmd entries
  
  case cached of
    Just entry | cacheExpiry entry > now -> 
      return $ cacheValue entry
    _ -> do
      result <- sendCommand handle cmd
      atomically $ modifyTVar cache $ 
        M.insert cmd (CacheEntry cmd result (addUTCTime 300 now))
      return result
```

### 6. プログレス表示

#### 長時間実行コマンドの進捗表示
```haskell
-- プログレス付きコマンド実行
runWithProgress :: UCMHandle -> Text -> (Text -> IO ()) -> IO Text
runWithProgress handle cmd progressCallback = do
  -- 別スレッドで出力を監視
  progressVar <- newTVarIO ""
  _ <- forkIO $ monitorProgress handle progressVar progressCallback
  
  result <- sendCommand handle cmd
  return result
```

## テスト戦略

### ユニットテスト
```haskell
-- test/Spec.hs
import Test.Hspec

spec :: Spec
spec = do
  describe "UCM Integration" $ do
    it "starts and stops UCM process" $ do
      withUCM testCodebase $ \handle -> do
        -- テスト実装
        
    it "handles command errors gracefully" $ do
      -- エラーケースのテスト
```

### 統合テスト
- 実際のUnisonコードベースを使用
- 各MCPツールの動作確認
- エラーケースの網羅的テスト

## 次のステップ

1. **今すぐ**: エラーハンドリングの改善を実装
2. **今週中**: pull/push/undo/redoコマンドを追加
3. **来週**: Unison Share API調査と基本実装
4. **再来週**: キャッシングとパフォーマンス改善

## 必要なリソース

### 外部ライブラリ
- `http-conduit`: HTTP通信用
- `aeson`: JSON処理
- `async`: 並行処理
- `stm`: トランザクショナルメモリ
- `containers`: 効率的なデータ構造

### 開発ツール
- `hspec`: テストフレームワーク
- `hlint`: コード品質チェック
- `haddock`: ドキュメント生成