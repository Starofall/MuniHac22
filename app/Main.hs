module Main(main) where

import LovecraftLetter.Core
import LovecraftLetter.ValidPlayableMoves

main :: IO ()
main = mapM_ print testPlayableMoves
  
testPlayableMoves :: [PlayableMove]
testPlayableMoves = listPlayableMoves round
  where
    round = RoundState deck 0 playerStates 
    playerStates = map (\p -> PlayerState [] [] Sane False False)  [0..2] 