# yomikatawa
A haskell CLI for https://yomikatawa.com

Python version (don't use it): https://github.com/tensorknower69/yomikatawa-py.

## Usage
### Help
```bash
$ yomikatawa -h
Usage: yomikatawa ((-v|--version) | [(-s|--sei) | (-m|--mei) | (-k|--kanji)]
                    INPUT_WORD [-r|--print-romaji] [-R|--print-random-words]
                    [-e|--print-same-reading-words])
  A haskell CLI for https://yomikatawa.com

Available options:
  -s,--sei                 Set category to 性
  -m,--mei                 Set category to 名
  -k,--kanji               Set category to 漢字, this is the default category
  -r,--print-romaji        Print romaji
  -R,--print-random-words  Print random words
  -e,--print-same-reading-words
                           Print words with same reading
  -h,--help                Show this help text
```

## Examples
### Search
```bash
$ yomikatawa -r -R -e 自身
Hiragana: じしん
Romaji: zisin
Same reading: 地震, 自信, 侍臣, 時針, 磁針
Random words: 齧り散らし, 優位性, 順三, 月並み, 秋雄, 立中, 林兼, 切瑳琢磨, 粋がっ, 黒阪
```

## Installation
```bash
$ git clone https://github.com/tensorknower69/yomikatawa
$ cd hs-yomikatawa
$ stack install
```
