-- | Random AI (RAI) Charles, playing not really randomly.

module RaiCharles (aiRaiCharles) where

import AI
import Yinsh

heuristic :: RaiCharles -> AIValue
heuristic _ = 42 -- yeah..

data RaiCharles = RaiCharles { gs :: GameState
                             , pl :: Int
                             }

instance AIPlayer RaiCharles where
    valueForWhite = heuristic
    getGamestate = gs
    getPlies = pl
    update ai gs' = ai { gs = gs' }

aiRaiCharles :: Int -> AIFunction
aiRaiCharles plies' gs' = aiTurn RaiCharles { gs = gs', pl = plies' }
