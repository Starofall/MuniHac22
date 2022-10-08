module Main(main) where

import LovecraftLetter.Core
import LovecraftLetter.ValidPlayableMoves
import LovecraftLetter.LovecraftDist
import qualified Text.Show.Pretty as Pretty

-- main :: IO ()
-- main = mapM_ print testPlayableMoves
  
-- testPlayableMoves :: [PlayableMove]
-- testPlayableMoves = listPlayableMoves round
--   where
--     round = RoundState deck 0 playerStates 
--     playerStates = map (\p -> PlayerState [] [] Sane False False)  [0..2] 

main = do
  Pretty.pPrint cardValue'
