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
                    AddMarker -> ringCoords' >>= newGS gs
                    RemoveRun -> runCoords' >>= newGS gs
                    PseudoTurn -> [fromJust (newGameState gs (0, 0))]
                    (MoveRing _) -> error "This is not supposed to happen"
                    RemoveRing -> error "This is not supposed to happen"
    where freeCoords = filter (freeCoord (board gs)) coords
          ringCoords' = ringCoords (board gs) (activePlayer gs)
          runCoords' = filter (partOfRun (markerCoords (board gs) (activePlayer gs))) coords -- TODO: we introduce an artificial branching here since we are generating too many moves (5 for a single run)
          newGS gs' c = case turnMode nextGS of
                            AddRing -> [nextGS]
                            AddMarker -> [nextGS]
                            RemoveRun -> [nextGS]
                            PseudoTurn -> [nextGS]
                            (MoveRing start) -> validRingMoves (board nextGS) start >>= newGS nextGS
                            RemoveRing -> ringCoords (board gs') (activePlayer nextGS) >>= newGS nextGS
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
          valueRings p = if rings p == ringsForWin
                         then hugeNumber
                         else 10000 * (rings p)
          rings p = if p == W then ringsW gs else ringsB gs
          valueMarkers p = length $ markerCoords (board gs) p

hugeNumber :: Int
hugeNumber = maxBound - 10 -- cannot use maxBound due to internals of the negamax implementation

terminalState :: GameState -> Bool
terminalState gs = ringsB gs == ringsForWin || ringsW gs == ringsForWin

instance GT.Game_tree GameState where
    is_terminal = terminalState
    node_value = heuristicValue
    children = gamestates

aiTurn :: GameState -> GameState
aiTurn gs = case turnMode gs of
                PseudoTurn -> fromJust $ newGameState gs (0, 0)
                _ -> pv !! 1
            where pv = fst $ NS.alpha_beta_search gs plies
                  plies = 3
