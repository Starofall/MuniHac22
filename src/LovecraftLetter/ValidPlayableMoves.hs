module LovecraftLetter.ValidPlayableMoves where

import LovecraftLetter.Core

data PlayableMove = PlayableMove
  { card :: Card,
    playCrazy :: Bool,
    target :: Maybe PlayerId,
    guess :: Maybe Int
    -- crazy 6 is complex
    --nyarReplaceOrder :: Map PlayerId [Card]
  }
  deriving (Eq, Ord, Show)

listPlayableMoves :: RoundState -> [PlayableMove]
listPlayableMoves rs = [(PlayableMove (head cards) False (Just 2) Nothing)]