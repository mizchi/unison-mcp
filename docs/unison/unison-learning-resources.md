# Unison 言語 学習資料リファレンス

## 公式リソース

- [Unison 公式サイト](https://www.unison-lang.org/)
- [公式ドキュメント](https://www.unison-lang.org/docs/)
- [Unison ツアー（チュートリアル）](https://www.unison-lang.org/docs/tour/)
- [Unison マニュアル（PDF）](https://unison-code.github.io/doc/manual.pdf)
- [GitHub リポジトリ](https://github.com/unisonweb/unison)

## インタラクティブ学習

- [Exercism Unison トラック](https://exercism.org/tracks/unison) - 51 の練習問題とメンタリングサポート

## コミュニティリソース

- [非公式 Abilities チュートリアル](https://gist.github.com/atacratic/7a91901d5535391910a2d34a2636a93c)

## インストールガイド

```bash
# ソースからのビルド
git clone https://github.com/unisonweb/unison.git
cd unison
stack build --fast --test && stack exec unison
```

## 基本概念

- コンテンツアドレス指定コード
- Abilities（エフェクトシステム）
- 分散コンピューティング
- UCM（Unison コードベースマネージャー）
