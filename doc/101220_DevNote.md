## MonadChip8開発記

### 2010-12-20

開発をはじめました．
年末年始休の暇潰しプロジェクトです．

### 2011-01-03

現在，CPUというデータ抽象型を引きまわしているのですが，これは正しくありません．
本来は，Stateというデータ抽象型を作り，それを引き回すように修正する予定です．
PPUレジスタもそこに含めてしまいます．

### 2012-02-21

性懲りもなく，また作り始めました．
次は，データを読み込む処理を書くつもりです．

_(2021-10-31) 2013年7月から2016年6月までは，「閑職」から「重職」になり，コードが書けませんでした．_
_2013年6月には，drawspriteのルーチンを実装していた記憶があるのですが，コードを紛失したため，再実装しています．_

### 2016-09-24

本日，2013年6月に書いた，drawspriteのルーチンを再度実装しました．
パドルが画面に出るようになりました．
でも，まだ点数表示はされません．
また，来週時間があれば，それをやることにします．

### 2016-09-25

2010年12月20日からhaskellで書いていたchip8のエミュレータが，やっと画面を表示するようになりました．
記録によると，2010年末から2011年始，2012年2月，2013年6月にマイブームが来てるみたいです．

chip8エミュレータは，新しい言語を覚えるために書いてみることが多く，2003年にc++，2005年にpythonで書きました．
ちなみに，[chip8](https://en.wikipedia.org/wiki/CHIP-8)は，1977年に開発されたcosmac vipで動く，インタプリタ言語です．

haskell版は，python版コードを見ながら翻訳しました．
haskellで同じものを書いてみた感想としては，以下6点です．

1. pythonでのオブジェクト指向的な記述は，haskellでもデータ抽象型を使うことで実現できます．
1. エミュレータ独特のビット操作は，Data.BitsやData.Wordで実現できます．
1. pythonのfor文は，リスト内包記法 [(x,y)| ...]と，末尾再帰で同じことができます．
1. do文を使えば，pythonの逐次処理も実現できます．
1. ただ，haskellは，型チェックが厳しく，生産性は高くないです．雰囲気で動いてくれればいいコードには向かないです．
    - 昔のgccとかだと，警告レベルを上げられましたが，ghcの場合，型チェックを下げられるとよいかもしれません．
1. haskellは書く人を選ぶ言語だと思います．

### 2016-09-26

emacsで，haskell-modeを使えるようにする方法です．

1. [melpa stable](https://melpa.org/#/haskell-mode)で，パッケージを"download"します．
1. haskell-mode-nn.tarみたいのを何処かに保存します．
1. emacsを管理者権限で起動します（右クリック）．
1. M-x package-install-fileで，さっきのファイル（haskell-mode-nn.tar）を指定します．
1. 勝手にインストールされます．
1. haskell-modeが使えるようになります．

### 2016-10-01

先週は朝1時間ぐらいを使って，0xFxxxの命令を実装していきました．
drawspriteを超えてしまえば，あとはなだらかな下り坂なのが，chip8エミュレータ作成です．
2タイマー（delay, sound）も実装し，CPUの上位にSystemというモジュールを作り，全体を制御することにしました．
1/60秒で，VSYNCという仮定で，タイマーをカウントダウンしていくようにしました．
少しコーディングした内容が間違っていたが，それも直して，点数表示が正しくされるようになりました．

### 2016-10-02

haskell版chip8エミュレータで，PONGを実行している画面です．
本日初めて，glossを使ってみました．概念を理解するまでに時間を要しました．
現時点では，vramの内容を表示しているだけです．
来週は，vramが変更されたら，表示も更新します．

![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/PONG2.png)

こちらは，ghci上で動作している画面です．
vramの内容を，asciiアートで表示していたのですが，本日，glossで画面実装してみました．

夜にも気になって，glossを調べていると，Graphics.Gloss.Interface.IO.Game.playIO という関数を発見しました．
これまでのモジュールとのグルーロジックを記述すると，見事に画面が表示されました．
キーイベントは未実装なので，それは来週やります．

### 2016-10-07

先週は，結構なコードを書きました
システム全体の動きを制御するSystem.hsを実装しました．
その上位モジュールとして，MonadChip8.hsも実装しました．
グラフィックモジュールとしては，glossを選びました．

### 2016-10-08

haskell版chip-8エミュレータですが，キーイベント処理を実装です．
glossを使えば，簡単にできます．
これでPONGのパドルが動くようになったので，ほぼ完成です．
あとはサウンドだけど，glossにサウンド処理あったかな．
ソースは希望があれば公開します．

### 2016-10-12

今は早めに帰宅できました．
haskell版chip8エミュレータに，別のROMイメージを食わせてみたら，結構動きました．
これは，VBRIXというブロック崩しのようなゲームです．
実は，結構面白いゲームです．

![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/VBRIX.png)

### 2016-10-15

haskell版chip8エミュレータの[ソース](https://github.com/jay-kumogata/MonadChip8)を公開しました．
ライブラリとして，[gloss](http://gloss.ouroborus.net/)が必要です．
'With cabal'に書いてある手順でインストールして下さい．
そして，コンパイル($ ghc MonadChip8.hs)して，実行($ MonadChip8 PONG)します．
著作権フリーの[ROM](http://www.zophar.net/pdroms/chip8/chip-8-games-pack.html)を利用下さい．

### 2016-10-23

wikiに記載したことを修正しました．
文体をですます調に変えて，多数存在した誤りを修正しました．
実現可能性については，ほぼ検証できたので，ひとまずは開発終了です．

### 2017-11-23

1年ぐらい前にHaskellで作ってたCHIP8エミュレータのスクリーンショット．なんか一昔前のLEDマトリックスパネルみたいな感じ．

![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/BRIX.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/TETRIS.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/SYZYGY.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/15PUZZLE.png)

### 2018-09-14

haskell版chip8エミュレータの[ソース](https://github.com/jay-kumogata/MonadChip8)を公開してます．ライブラリとして，[gloss](http://gloss.ouroborus.net)が必要です．'With cabal'でインストールして下さい．そして，コンパイル('> ghc MonadChip8.hs')して，実行('> MonadChip8 PONG')します．

### 2018-10-26

gloss-1.13.0.1に更新して，再度リンクしました．
画面描画が速くなったような気がします．気のせいかもしれません．

### 2019-09-27

スクリーンショットを上げました．さらに，markdownでの記載を修正しました．

![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/HIDDEN.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/MAZE.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/MERLIN.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/MISSLE.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/TANK.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/TICTAC.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/VERS.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/UFO.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/WIPEOUT.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/PUZZLE.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/INVADERS.png)

### 2020-03-23

さらに，markdownでの記載を修正しました．

### 2021-03-27

stack環境に移行しました．

	> stack setup
	> stack install random
	> stack install gloss
	> stack ghc MonadChip8.hs
	> ./MonadChip8 PONG

windows環境の場合，下記手順が必要です．
cf. https://stackoverflow.com/questions/42072958/haskell-with-opengl-unknown-glut-entry-glutinit

自作ゲームも多少調節したら動くようになりました．

![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/AMABIE01.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/AMABIE03.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/AMABIE04.png)
![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/AMABIE07.png)

### 2021-04-04

パッケージングシステムstackに移行しました．
octoで作ったオリジナルゲームamabieを動作させました．

### 2022-01-08

Chip8命令のテストコードをOcto記法で実装した方がいらっしゃるようで，MonadChip8でも動かしてみました．すべてOKでした👏👏👏． #Chip8 #Octo #Haskell cf. https://github.com/corax89/chip8-test-rom

![](https://github.com/jay-kumogata/MonadChip8/blob/master/screenshots/test_opcode.png)

以上
