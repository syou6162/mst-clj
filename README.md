# mst-clj

A Clojure library designed to ... well, that part is up to you.

## Usage

### [CoNll2009形式](http://ufal.mff.cuni.cz/conll2009-st/task-description.html)の教師データ、テストデータをMSTのファイルフォーマット(ラベルあり形式)に変換

```sh
python bin/conllIX2mst.py ~/Desktop/CoNLL2009-ST-English-train.txt > conll2009mst.txt

```

実際に使うときには`lein run -m mst-clj.core -h`で指定できるオプションを見てみるとよい。並列していくつかのモデルを学習するときには`--model-filename`オプションや`--feature-to-id-filenam`を変更するべし。

### 学習

```sh
lein run --mode train --filename conll2009mst.txt --max-iter 10
```

### 評価

```sh
lein run --mode eval --filename conll2009mst.txt
```

## Notice

- CoNLL2009のフォーマットを仮定しており、品詞情報には正解データを使用します
- メモリを大量に食います(特に学習時)。35000文程度の学習で25GB前後
 - テスト時にも素性とidの変換のテーブルがメモリを食います
- MIRAではなくstructured perceptronで構造学習を行なっている
- 2単語間の間にある単語の品詞を使ったトライグラムの品詞は未実装
- decodeは並列にやっているため、single threadで動かすともっと遅いです
- テスト文の入力をCoNLL2009形式で吐くように...したい

参考までに学習後の素性数と精度はこんな感じ(テスト文数は1000文)。completeの精度が低い。

### 1000文

> Number of features:  281599
> 0.7801653537165588
> 0.098

### 3000文

> Number of features:  629749
> 0.8134310896185944
> 0.142

## License

Copyright © 2012 Yasuhisa Yoshida

Distributed under the Eclipse Public License, the same as Clojure.
