# universe

[Universe simulation](https://github.com/luke-tangh/universe) in Haskell.

![Alt Text](example.gif)

## Environment
- `Ubuntu 22.04.3`
- `gloss 1.13.2.2`

## Run
Install dependencies:
```
sudo apt-get install freeglut3 freeglut3-dev libgmp3-dev
cabal install --lib gloss random random-shuffle
```
Generate output:
```
ghc -O2 -threaded -dynamic Main.hs
```
Run:
```
./Main
```

## Appreciation
Inspired by [Haskell-gravity-simulation](https://github.com/Coayer/Haskell-gravity-simulation).
