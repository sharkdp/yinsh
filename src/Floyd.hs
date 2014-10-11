-- | Simply the best AI for Yinsh, seriously.

module Floyd ( aiFloyd
             , mhNumber
             , rhRingMoves
             , rhConnected
             , rhZero
             )
    where

import AI
import Yinsh

-- TODO: should we care which turn mode we are in? -> Yes, PseudoTurn can be evaluated..
-- TODO: adjust numbers: 5, 10
floydHeuristic :: Floyd -> AIValue
floydHeuristic ai | points W >= pointsForWin = hugeNumber
                  | points B >= pointsForWin = -hugeNumber
                  | otherwise                = value W - value B
    where gs' = getGamestate ai
          points W = pointsW gs'
          points B = pointsB gs'
          board' = board gs'

          valueMarkers = markerH ai
          valueRings = ringH ai

          value p = valuePoints         p
                  + valueMarkers board' p
                  + valueRings   board' p

          valuePoints p = 100000 * points p

type MarkerHeuristic = Board -> Player -> AIValue
type RingHeuristic   = Board -> Player -> AIValue

mhNumber :: MarkerHeuristic
mhNumber b p = (10 *) $ length $ markers p b

rhRingMoves :: RingHeuristic
rhRingMoves b p = (1 *) $ sum $ map (length . ringMoves b) $ rings p b

rhConnected :: RingHeuristic
rhConnected b p = (1 *) $ length $ filter connectedToRings coords
     where connectedToRings c = any (c `connected`) (rings p b)

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

aiFloyd :: Int -> MarkerHeuristic -> RingHeuristic -> AIFunction
aiFloyd plies' mh' rh' gs' = aiTurn Floyd { gs = gs'
                                          , plies = plies'
                                          , markerH = mh'
                                          , ringH = rh'
                                          }
