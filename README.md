# atmaCup_16 14th place solution

* atmacup#16の14th place solution コード
* 解法の概要は[こちら](https://www.guruguru.science/competitions/22/discussions/5917e834-4a9d-4d71-8c0d-a03fa1b90311/)
* 今回ルールベースだったのでPythonではなくRを利用してます(普段はデータ確認・EDAはR、機械学習のパイプラインはPythonという使い分け)

## 実行環境
```
> sessionInfo()
R version 4.2.2 (2022-10-31)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Monterey 12.3.1
```

## 提出ファイル作成

* `data/raw`フォルダに[コンペデータ](https://www.guruguru.science/competitions/22/data-sources)を配置
* 以下のコマンドを実行することで`data/sub`フォルダに`sub.csv`が作成
```
source("script/rule_1.R", echo=TRUE)
source("script/rule_2.R", echo=TRUE)
source("script/rule_3.R", echo=TRUE)
source("script/rule_4.R", echo=TRUE)
source("script/rule_5.R", echo=TRUE)
source("script/rule_6.R", echo=TRUE)
source("script/rule_7.R", echo=TRUE)
source("script/ensemble.R", echo=TRUE)
```

