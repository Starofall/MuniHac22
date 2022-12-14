module LovecraftLetter.ValidPlayableMoves where

import LovecraftLetter.Core
import Numeric.Natural
import GHC.Natural (naturalToInteger)
import Data.List (nub)

data PlayableMove = PlayableMove
  { card :: Card,
    playSanity :: Sanity,
    target :: Maybe PlayerId,
    guess :: Maybe Int
    -- crazy 6 is complex
    --nyarReplaceOrder :: Map PlayerId [Card]
  }
  deriving (Eq, Ord, Show)

listPlayableMoves :: RoundState -> [PlayableMove]
listPlayableMoves rs@(RoundState _ currentPlayerId playerStates) =
  nub $ currentPlayerCards >>= (listCardPlayableMoves rs)
  where
    PlayerState currentPlayerCards bord sane _ _ = getCurrentPlayerState rs


getStateForPlayer :: RoundState -> PlayerId -> PlayerState
getStateForPlayer (RoundState _ _ playerStates) pid =
  playerStates !! fromIntegral (naturalToInteger pid)


getCurrentPlayerState :: RoundState -> PlayerState
getCurrentPlayerState rs@(RoundState _ currentPlayerId _) =
 getStateForPlayer rs currentPlayerId

-- lists all options but does not take care of the 7 card corner case
listCardPlayableMoves :: RoundState -> Card -> [PlayableMove]
listCardPlayableMoves rs c =
  case (cardValue c, cardSanity c) of
    (1, Sane) -> [PlayableMove c Sane (Just $ fromInteger $ toInteger playerId) (Just guess)
      | guess <- [2..8], playerId <- otherPlayerIds] -- for each player that is not me, 2-8
    (2, Sane) -> [PlayableMove c Sane (Just $ fromInteger $ toInteger playerId) Nothing | playerId <- otherPlayerIds]
    (3, Sane) -> [PlayableMove c Sane (Just $ fromInteger $ toInteger playerId) Nothing | playerId <- otherPlayerIds]
    (4, Sane) -> [PlayableMove c Sane Nothing Nothing]
    (5, Sane) -> [PlayableMove c Sane (Just $ fromInteger $ toInteger playerId) Nothing | playerId <- otherPlayerIds]
    (6, Sane) -> [PlayableMove c Sane (Just $ fromInteger $ toInteger playerId) Nothing | playerId <- otherPlayerIds]
    (7, Sane) -> [PlayableMove c Sane Nothing Nothing]
    (8, Sane) -> [PlayableMove c Sane Nothing Nothing]
    (0, Insane) -> [(PlayableMove c Insane Nothing Nothing), (PlayableMove c Sane Nothing Nothing)]
    (1, Insane) -> [PlayableMove c sane (Just $ fromInteger $ toInteger playerId) (Just guess) | guess <- [2..8], playerId <- otherPlayerIds, sane <- [Sane,Insane]]
    (2, Insane) -> [PlayableMove c sane (Just $ fromInteger $ toInteger playerId) Nothing | playerId <- otherPlayerIds, sane <- [Sane,Insane]]
    (3, Insane) -> [PlayableMove c sane (Just $ fromInteger $ toInteger playerId) Nothing | playerId <- otherPlayerIds, sane <- [Sane,Insane]]
    (4, Insane) -> [(PlayableMove c Insane Nothing Nothing), (PlayableMove c Sane Nothing Nothing)]
    (5, Insane) -> [PlayableMove c sane (Just $ fromInteger $ toInteger playerId) Nothing | playerId <- otherPlayerIds, sane <- [Sane,Insane]]
    (6, Insane) -> [PlayableMove c Insane Nothing Nothing] ++ [PlayableMove c Sane (Just $ fromInteger $ toInteger playerId) Nothing | playerId <- otherPlayerIds]
    (7, Insane) -> [(PlayableMove c Sane Nothing Nothing),(PlayableMove c Insane Nothing Nothing)]
    (8, Insane) -> [(PlayableMove c Sane Nothing Nothing),(PlayableMove c Insane Nothing Nothing)]
    _ -> error "Invalid"
  where
    playerStates = roundPlayerStates rs
    currentPlayerId = fromIntegral $ roundCurrentPlayer rs
    otherPlayerIds = filter (/= currentPlayerId) [0..((length playerStates)-1)]

