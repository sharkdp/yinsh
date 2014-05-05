module Floyd where

import Data.Maybe (fromJust)
import qualified Data.Tree.Game_tree.Game_tree as GT
import qualified Data.Tree.Game_tree.Negascout as NS

import Yinsh

-- | Possible new game states. The input and output game states are guaranteed
-- to be in turn mode AddRing, AddMarker, RemoveRun or PseudoTurn.
gamestates :: GameState -> [GameState]
gamestates gs = case turnMode gs of
                    AddRing -> freeCoords >>= newGS gs
                    AddMarker -> rings' >>= newGS gs
                    RemoveRun -> runCoords' >>= newGS gs
                    PseudoTurn -> [fromJust (newGameState gs (0, 0))]
                    (MoveRing _) -> error "This is not supposed to happen"
                    RemoveRing -> error "This is not supposed to happen"
    where freeCoords = filter (freeCoord (board gs)) coords
          rings' = rings (activePlayer gs) (board gs)
          runCoords' = filter (partOfRun (markers (activePlayer gs) (board gs))) coords -- TODO: we introduce an artificial branching here since we are generating too many moves (5 for a single run)
          newGS gs' c = case turnMode nextGS of
                            AddRing -> [nextGS]
                            AddMarker -> [nextGS]
                            RemoveRun -> [nextGS]
                            PseudoTurn -> [nextGS]
                            (MoveRing start) -> validRingMoves (board nextGS) start >>= newGS nextGS
                            RemoveRing -> rings (activePlayer nextGS) (board gs') >>= newGS nextGS
                        where nextGS = fromJust $ newGameState gs' c

-- | Heuristic evaluation function for a game state. Everything is calulated
-- from the perspective of the AI player (white). The 'sign' factor is used
-- for the negamax algorithm (Yinsh is a zero sum game).
heuristicValue :: GameState -> Int
heuristicValue gs = sign * valueForWhite -- TODO: should we care which turn mode we are in?
    where sign | activePlayer gs == W = 1
               | otherwise            = -1
          valueForWhite =   valueRings W - valueRings B
                          + valueMarkers W - valueMarkers B
          valueRings p = if points p == pointsForWin
                         then hugeNumber
                         else 10000 * (points p)
          points W = pointsW gs
          points B = pointsB gs
          valueMarkers p = length $ markers p (board gs)

hugeNumber :: Int
hugeNumber = maxBound - 10 -- cannot use maxBound due to internals of the negamax implementation

terminalState :: GameState -> Bool
terminalState gs = pointsB gs == pointsForWin || pointsW gs == pointsForWin

instance GT.Game_tree GameState where
    is_terminal = terminalState
    node_value = heuristicValue
    children = gamestates

plies :: Int
plies = 5

aiTurn :: GameState -> GameState
aiTurn gs = case turnMode gs of
                PseudoTurn -> fromJust $ newGameState gs (0, 0)
                _ -> pv !! 1
            where pv = fst $ NS.alpha_beta_search gs plies
