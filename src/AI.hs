{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AI where

import qualified Data.Tree.Game_tree.Game_tree as GT
import qualified Data.Tree.Game_tree.Negascout as NS

import Data.Maybe (fromJust)
import Data.List (nubBy, sort)

import Yinsh

-- | Every AI should provide a function @ai..@, returning an AIFunction.
type AIFunction = GameState -> GameState

-- | Result of an heuristic evaluation function
type AIValue = Int

-- | Wrapper class for AI players which encapsules the current game state.
class AIPlayer a where
    -- | Heuristic evaluation function for a game state. Everything is calulated
    -- from the perspective of the white player. This is sufficient since
    -- Yinsh is a zero sum game.
    valueForWhite :: a -> AIValue

    -- | Number of turns to look ahead in the game tree.
    getPlies :: a -> Int

    -- | Unwrap the gamestate inside the AI.
    getGamestate :: a -> GameState

    -- | Update AI with new gamestate
    update :: a -> GameState -> a

-- | Make the GameState (wrapped in the AIPlayer) an instance of Game_tree
-- (actually rather an instance of a node in the game tree).
instance (AIPlayer a) => GT.Game_tree a where
    is_terminal = terminalState . getGamestate
    children ai = map (update ai) (gamestates (getGamestate ai))
    node_value ai = sign * valueForWhite ai
        where sign | activePlayer gs == W = 1
                   | otherwise            = -1
              gs = getGamestate ai

-- | Possible new game states. The input and output game states are guaranteed
-- to be in turn mode AddRing, AddMarker, RemoveRun or PseudoTurn.
gamestates :: GameState -> [GameState]
gamestates gs | terminalState gs = []
              | otherwise =
                    case turnMode gs of
                        AddRing -> freeCoords >>= newGS gs
                        AddMarker -> rings' >>= newGS gs
                        RemoveRun -> runCoords' >>= newGS gs
                        PseudoTurn -> [fromJust (newGameState gs (0, 0))]
                        (MoveRing _) -> error "This is not supposed to happen"
                        RemoveRing -> error "This is not supposed to happen"
    where freeCoords = filter (freeCoord (board gs)) coords -- TODO: factor out, optimize
          rings' = rings (activePlayer gs) (board gs)
          runCoords' = removeDups $ filter (partOfRun markers') coords
          markers' = markers (activePlayer gs) (board gs)
          removeDups = nubBy (\c1 c2 -> sort (runCoords markers' c1) == sort (runCoords markers' c2))
          newGS gs' c = case turnMode nextGS of
                            AddRing -> [nextGS]
                            AddMarker -> [nextGS]
                            RemoveRun -> [nextGS]
                            PseudoTurn -> [nextGS]
                            (MoveRing start) -> ringMoves (board nextGS) start >>= newGS nextGS
                            RemoveRing -> rings (activePlayer nextGS) (board gs') >>= newGS nextGS
                        where nextGS = fromJust $ newGameState gs' c

-- | Get new game state after the AI turn.
aiTurn :: (AIPlayer ai) => ai -> GameState
aiTurn ai = case turnMode gs of
                PseudoTurn -> fromJust $ newGameState gs (0, 0)
                _ -> pv !! 1
            where pv = aiPV ai
                  gs = getGamestate ai

-- | Get the whole principal variation.
aiPV :: (AIPlayer ai) => ai -> [GameState]
aiPV ai = map getGamestate gss
    where (gss, _) = NS.negascout ai ply
          ply = getPlies ai
-- TODO: negascout really seems to be the fastest. But test this for more game states
-- NS.alpha_beta_search gs plies'
-- NS.principal_variation_search gs plies'

-- | A large number for symbolizing a win (maxBound does *not* work here due
-- to restrictions in the gametree module.)
hugeNumber :: Int
hugeNumber = 10^15
