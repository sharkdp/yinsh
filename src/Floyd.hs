-- | Simply the best AI for Yinsh, seriously.

module Floyd where

import AI
import Yinsh

floydHeuristic :: Floyd -> Int
floydHeuristic ai | points W == pointsForWin = hugeNumber
                  | points B == pointsForWin = -hugeNumber
                  | otherwise = value W - value B
    where gs = getGamestate ai
          value p = valuePoints p + valueMarkers p + valueRings p
          valuePoints p = 10000 * points p
          valueRings p = (1 *) $ sum $ map (length . ringMoves board') $ rings p board'
          -- valueRings p = (1 *) $ length $ filter (connectedToRings p) coords
          -- connectedToRings p c = any (c `connected`) (rings p board')
          -- valueRings p = 0
          points W = pointsW gs
          points B = pointsB gs
          valueMarkers p = (10 *) $ length $ markers p board'
          board' = board gs

-- TODO: should we care which turn mode we are in? -> Yes, PseudoTurn can be evaluated..
-- TODO: adjust numbers: 5, 10

data Floyd = Floyd { floydGS :: GameState
                   , floydPL :: Int
                   }

instance AIPlayer Floyd where
    valueForWhite = floydHeuristic
    getGamestate = floydGS
    getPlies = floydPL
    update ai gs = ai { floydGS = gs }
    name _ = "Floyd"
