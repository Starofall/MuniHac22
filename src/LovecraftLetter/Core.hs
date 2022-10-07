module LovecraftLetter.Core where

import Data.Text(Text)
import Numeric.Natural(Natural)
import LovecraftLetter.Operational
import Control.Monad (when)
import Data.Function (on)
import Control.Monad.State.Strict (StateT, MonadState)
import Control.Monad.Except (MonadError)
import Control.Monad.State (get)

type PlayerId = Natural

data Sanity
  = Sane
  | Insane
  deriving (Eq, Ord, Show)

data Player =
  Player
  { playerId :: PlayerId
  , playerWins :: [Sanity]
  }
  deriving (Eq, Ord, Show)

data GlobalState =
  GlobalState
  { globalPlayers :: [Player]
  , globalDeck :: [Card]
  , globalNextRoundFirstPlayer :: PlayerId
  }
  deriving (Eq, Ord, Show)

data RoundState =
  RoundState
  { roundDeck :: [Card]
  , roundCurrentPlayer :: PlayerId
  , roundPlayerStates :: [PlayerState]
  }
  deriving (Eq, Ord, Show)

data PlayerState =
  PlayerState
  { playerHand :: [Card]
  , playerBoard :: [Card]
  , playerSanity :: Sanity
  , playerRoundProtected :: Bool
  , playerGameProtected :: Bool
  }
  deriving (Eq, Ord, Show)

data Card =
  Card
  { cardName :: Text
  , cardValue :: Natural
  , cardSanity :: Sanity
  , cardEffect :: CardEffect
  }

instance Eq Card where
  (==) = (==) `on` cardName

instance Ord Card where
  compare = compare `on` cardName

instance Show Card where
  show = show . cardName

data Action a where
  Die :: PlayerId -> Action ()
  Draw :: PlayerId -> Action ()
  Guess :: PlayerId -> Natural -> Action Bool
  Inspect :: PlayerId -> Action Card
  ProtectRound :: PlayerId -> Action ()
  ProtectGame :: PlayerId -> Action ()
  Play :: PlayerId -> Action ()
  Swap :: PlayerId -> PlayerId -> Action ()
  Inform :: PlayerId -> Knowledge -> Action ()

data Knowledge
  = PlayerHasCard PlayerId Card

type CardEffect = PlayerId -> PlayerId -> Natural -> Program Action ()

eff1 :: CardEffect
eff1 _player target guess = do
  guess <- perform $ Guess target guess
  when guess do
    perform $ Die target

eff2 :: CardEffect
eff2 player target _guess = do
  card <- perform $ Inspect target
  perform $ Inform player $ PlayerHasCard target card

eff3 :: CardEffect
eff3 player target _guess = do
  playerCard <- perform $ Inspect player
  targetCard <- perform $ Inspect target

  case compare playerCard targetCard of
    EQ -> pure ()
    LT -> perform $ Die player
    GT -> perform $ Die target

eff4 :: CardEffect
eff4 player _target _guess = perform $ ProtectRound player

eff5 :: CardEffect
eff5 _player target _guess = do
  perform $ Play target
  perform $ Draw target

newtype Game a = Game { runGame :: StateT RoundState (Either Text) a }
  deriving newtype (Functor, Applicative, Monad, MonadState RoundState, MonadError Text)

interpretAction :: Program Action a -> Game a
interpretAction = interpret \case
  Inspect p -> do
    roundState <- get
    pure $ head $ playerHand (roundPlayerStates roundState !! fromIntegral p)

  _ -> undefined

  -- (Die p) -> _wa
  -- (Draw p) -> _wb
  -- (Guess p p') -> _wc
  -- (ProtectRound p) -> _we
  -- (ProtectGame p) -> _wf
  -- (Play p) -> _wg
  -- (Swap p p') -> _wh
  -- (Inform p knowl) -> _wi

cards :: [Card]
cards =
  [ Card "Investigators" 1 Sane eff1
  , Card "Cats of Ulthaar" 2 Sane eff2
  , Card "Great Race of Yith" 3 Sane eff3
  , Card "Elder Sign" 4 Sane eff4
  , Card "Prof. Henry Armitage" 5 Sane eff5
  ]
