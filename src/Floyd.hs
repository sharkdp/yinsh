-- | Simply the best AI for Yinsh, seriously.

module Floyd ( aiFloyd
             , mhNumber
             , rhRingMoves
             , rhConnected
             , rhControlledMarkers
             , rhCombined
             , rhZero
             )
    where

import AI
import Yinsh

-- TODO: adjust numbers: 5, 10
floydHeuristic :: Floyd -> AIValue
floydHeuristic ai | points' W >= pointsForWin = hugeNumber
                  | points' B >= pointsForWin = -hugeNumber
                  | otherwise                 = value W - value B
    where gs' = getGamestate ai
          board' = board gs'
          ap' = activePlayer gs'
          tm' = turnMode gs'
          points W = pointsW gs'
          points B = pointsB gs'

          points' p = points p + futurePoints p
          -- If we are in RemoveRun phase, already include the point for the
          -- active player.
          -- If we are is WaitRemoveRun, the *opponent* of the current player
          -- will necessarily have one more point next turn.
          futurePoints p = case tm' of
                               (RemoveRun _)     -> if ap' == p then 1 else 0
                               (WaitRemoveRun _) -> if ap' == p then 0 else 1
                               _                 -> 0

          valuePoints p = 100000 * points' p
          valueMarkers =  markerH ai
          valueRings =    ringH ai

          value p = valuePoints         p
                  + valueMarkers board' p
                  + valueRings   board' p


type MarkerHeuristic = Board -> Player -> AIValue
type RingHeuristic   = Board -> Player -> AIValue

mhNumber :: MarkerHeuristic
mhNumber b p = (10 *) $ length $ markers p b

rhRingMoves :: RingHeuristic
rhRingMoves b p = (1 *) $ sum $ map (length . ringMoves b) $ rings p b

rhConnected :: RingHeuristic
rhConnected b p = (1 *) $ length $ filter connectedToRings coords
     where connectedToRings c = any (c `connected`) (rings p b)

rhControlledMarkers :: RingHeuristic
rhControlledMarkers b p = sum $ map controlledM (rings p b)
    where controlledM :: YCoord -> Int
          controlledM start = sum $ map markersBetween endPos
              where endPos = ringMoves b start
                    markersBetween end = length $ filter (isMarker b) $ coordLine start end

rhCombined :: [(Int, RingHeuristic)] -> RingHeuristic
rhCombined list b p = sum $ zipWith (*) points vs
    where (vs, hs) = unzip list
          points = map (\h -> h b p) hs

rhZero :: RingHeuristic
rhZero _ _ = 0

data Floyd = Floyd { gs :: GameState
                   , plies :: Int
                   , markerH :: MarkerHeuristic
                   , ringH :: RingHeuristic
                   }

instance AIPlayer Floyd where
    valueForWhite = floydHeuristic
    getGamestate = gs
    getPlies = plies
    update ai gs' = ai { gs = gs' }

mkFloyd :: Int -> MarkerHeuristic -> RingHeuristic -> GameState -> Floyd
mkFloyd plies' mh' rh' gs' = Floyd { gs = gs'
                                   , plies = plies'
                                   , markerH = mh'
                                   , ringH = rh'
                                   }

aiFloyd :: Int -> MarkerHeuristic -> RingHeuristic -> AIFunction
aiFloyd plies' mh' rh' gs' = aiTurn $ mkFloyd plies' mh' rh' gs'
