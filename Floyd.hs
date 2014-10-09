module Floyd where

import Data.Maybe (fromJust)
import qualified Data.Tree.Game_tree.Game_tree as GT
import qualified Data.Tree.Game_tree.Negascout as NS
import Data.List (nubBy, sort)

import Yinsh

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
                            (MoveRing start) -> validRingMoves (board nextGS) start >>= newGS nextGS
                            RemoveRing -> rings (activePlayer nextGS) (board gs') >>= newGS nextGS
                        where nextGS = fromJust $ newGameState gs' c

-- | Heuristic evaluation function for a game state. Everything is calulated
-- from the perspective of the AI player (white). The 'sign' factor is used
-- for the negamax algorithm (Yinsh is a zero sum game).
heuristicValue :: GameState -> Int
heuristicValue gs = sign * valueForWhite -- TODO: should we care which turn mode we are in? -> Yes, PseudoTurn can be evaluated..
    where sign | activePlayer gs == W = 1
               | otherwise            = -1
          valueForWhite | points W == pointsForWin = hugeNumber
                        | points B == pointsForWin = -hugeNumber
                        | otherwise = value W - value B
          value p = valuePoints p + valueMarkers p + valueRings p
          valuePoints p = 10000 * points p
          -- TODO: this ring-heuristic is too expensive:
          valueRings p = (1 *) $ sum $ map (length . validRingMoves board') $ rings p board'
          -- valueRings p = (1 *) $ length $ filter (connectedToRings p) coords
          -- valueRings p = sum $ map (rings p board')
          connectedToRings p c = any (c `connected`) (rings p board')
          points W = pointsW gs
          points B = pointsB gs
          valueMarkers p = (10 *) $ length $ markers p board'
          board' = board gs
          -- TODO: adjust these numbers: 5, 10

hugeNumber :: Int
hugeNumber = 10000000000000

instance GT.Game_tree GameState where
    is_terminal = terminalState
    node_value = heuristicValue
    children = gamestates

plies :: Int
plies = 2

aiTurn :: Int -> GameState -> GameState
aiTurn plies' gs = case turnMode gs of
                PseudoTurn -> fromJust $ newGameState gs (0, 0)
                _ -> pv !! 1
            where pv = fst $ aiRes plies' gs

aiRes :: Int -> GameState -> ([GameState], Int)
aiRes plies' gs = NS.negascout gs plies'
-- TODO: negascout really seems to be the fastest. But test this for more game states
-- NS.alpha_beta_search gs plies'
-- NS.principal_variation_search gs plies'
