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
  WinGame :: PlayerId -> Action () -- chutulu with 2 cracy cards
  Die :: PlayerId -> Action ()
  Draw :: PlayerId -> Action ()
  Guess :: PlayerId -> Natural -> Action Bool
  Inspect :: PlayerId -> Action Card
  ProtectRound :: PlayerId -> Action ()
  ProtectGame :: PlayerId -> Action ()
  Play :: PlayerId -> Action ()
  TakeCard :: PlayerId -> Action Card
  GiveMiGo :: PlayerId -> Action ()
  Swap :: PlayerId -> PlayerId -> Action ()
  Inform :: PlayerId -> Knowledge -> Action ()

data Knowledge
  = PlayerHasCard PlayerId Card

type CardEffect = PlayerId -> PlayerId -> Natural -> Program Action ()


eff0 :: CardEffect
eff0 player _target _guess = do
  perform $ Die player

effC0 :: CardEffect
effC0 player _target _guess = do
  perform $ Die player


eff1 :: CardEffect
eff1 _player target guess = do
  guess <- perform $ Guess target guess
  when guess do
    perform $ Die target

effC1 :: CardEffect
effC1 _player target guess = do
  isOne <- perform $ Guess target 1
  correct <- perform $ Guess target guess
  when (correct || isOne) do
    perform $ Die target

eff2 :: CardEffect
eff2 player target _guess = do
  card <- perform $ Inspect target
  perform $ Inform player $ PlayerHasCard target card

effC2 :: CardEffect
effC2 player target _guess = do
  card <- perform $ Inspect target
  perform $ Inform player $ PlayerHasCard target card
  perform $ Draw player
  -- perform $ DoRound player ???
  -- @todo we miss the play feature

eff3 :: CardEffect
eff3 player target _guess = do
  playerCard <- perform $ Inspect player
  targetCard <- perform $ Inspect target
  case compare playerCard targetCard of
    EQ -> pure ()
    LT -> perform $ Die player
    GT -> perform $ Die target

effC3 :: CardEffect
effC3 _player target _guess = do
  perform $ Die target
  -- @ missing board access to check if he is cracy

eff4 :: CardEffect
eff4 player _target _guess = perform $ ProtectRound player

effC4 :: CardEffect
effC4 player _target _guess = perform $ ProtectGame player


eff5 :: CardEffect
eff5 _player target _guess = do
  perform $ Play target
  perform $ Draw target

effC5 :: CardEffect
effC5 _player target _guess = do
  card <- perform $ TakeCard target
  perform $ GiveMiGo target
--  perform $ DoRound _player


eff6 :: CardEffect
eff6 player target _guess = do
  perform $ Swap player target

effC6 :: CardEffect
effC6 _player _target _guess = do
  pure () -- missing!
  -- perform cracyness!

eff7 :: CardEffect
eff7 _player _target _guess = do
  pure()

effC7 :: CardEffect
effC7 _player _target _guess = do
  pure() -- @todo
  -- needs playerstate access ->

eff8 :: CardEffect
eff8 player _target _guess = do
  perform $ Die player

effC8 :: CardEffect
effC8 player _target _guess = do
  perform $ Die player -- @todo
  -- needs playerstate access


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
  , Card "Randolph Carter" 6 Sane eff6
  , Card "The Silver Key" 7 Sane eff7
  , Card "Necronomicon" 8 Sane eff8
  , Card "Mi-Go BrainCase" 0 Insane effC0
  , Card "Deep Ones" 1 Insane effC1
  , Card "Golden Mead" 2 Insane effC2
  , Card "Hound of Tindalos" 3 Insane effC3
  , Card "Liber Ivonis" 4 Insane effC4
  , Card "Mi-Go" 5 Insane effC5
  , Card "Nyarlathotep" 6 Insane effC6
  , Card "The Shinig Trapezohedron" 7 Insane effC7
  , Card "Cthulhu" 8 Insane effC8
  ]

cardAmount :: Card -> Int
cardAmount (Card _ 1 Sane _) = 5
cardAmount (Card _ 2 Sane _) = 2
cardAmount (Card _ 3 Sane _) = 2
cardAmount (Card _ 4 Sane _) = 2
cardAmount (Card _ 5 Sane _) = 2
cardAmount (Card _ 6 Sane _) = 1
cardAmount (Card _ 7 Sane _) = 1
cardAmount (Card _ 8 Sane _) = 1
cardAmount (Card _ _ Insane _) = 1
cardAmount _ = 0

cardReplicated :: Card -> [Card]
cardReplicated card = replicate (cardAmount card) card

deck :: [Card]
deck = cards >>= cardReplicated