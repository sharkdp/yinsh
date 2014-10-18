-- | Trying to complete with the best

module Pink ( aiPink
             -- , mhNumber
             -- , rhRingMoves
             -- , rhConnected
             -- , rhZero
             )
    where

import AI
import Yinsh

pinkHeuristic :: Pink -> AIValue
pinkHeuristic ai | points W >= pointsForWin = hugeNumber
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

-- rhControlledMarkers :: RingHeuristic
-- rhControlledMarkers b p = length $ nub $ controlledM =<< (rings p b)
--     controlledM :: YCoord -> [YCoord]
--     controlledM c = ringMoves b c

rhZero :: RingHeuristic
rhZero _ _ = 0

data Pink = Pink { gs :: GameState
                   , plies :: Int
                   , markerH :: MarkerHeuristic
                   , ringH :: RingHeuristic
                   }

instance AIPlayer Pink where
    valueForWhite = pinkHeuristic
    getGamestate = gs
    getPlies = plies
    update ai gs' = ai { gs = gs' }

aiPink :: Int -> MarkerHeuristic -> RingHeuristic -> AIFunction
aiPink plies' mh' rh' gs' = aiTurn Pink { gs = gs'
                                          , plies = plies'
                                          , markerH = mh'
                                          , ringH = rh'
                                          }
