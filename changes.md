# Changes in Version 0.0.3

- between pos-tagを素性に追加
- pos trigramを素性に追加
- 重みの学習方法としてAveraged Structured Perceptronを採用
 - augmented lossを取り入れて、積極的に誤りに対して重みの更新を行なう
- 重みをhash-mapではなくJavaの配列で持つように変更
- negative featureを作らないように変更
- lemmaとcoarse pos-tagを素性に追加
- 表層が数字のものを正規化
