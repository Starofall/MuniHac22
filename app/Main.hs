module Main(main) where

import LovecraftLetter.Core
import LovecraftLetter.ValidPlayableMoves

main :: IO ()
main = do
  shuffledDeck <- shuffle deckWithoutZero
  let global = initGlobal 3 shuffledDeck
  let round = initRound global
  mapM_ print $ listPlayableMoves round

testPlayableMoves :: [PlayableMove]
testPlayableMoves = listPlayableMoves round
  where
    round = RoundState deckWithoutZero 0 playerStates
    playerStates = map (\p -> PlayerState deckWithoutZero [] Sane False False)  [0..2]
    --(take 2 $ filter (\x -> cardValue x == 4) deck)