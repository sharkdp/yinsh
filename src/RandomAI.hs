-- | Random AI for Yinsh.

module RandomAI where

import AI
import Yinsh

heuristic :: RandomAI -> Int
heuristic _ = 42 -- yeah.. it is not really random

data RandomAI = RandomAI { raiGS :: GameState
                         , raiPL :: Int
                         }

instance AIPlayer RandomAI where
    valueForWhite = heuristic
    getGamestate = raiGS
    getPlies = raiPL
    update ai gs = ai { raiGS = gs }
    name _ = "RandomAI"
