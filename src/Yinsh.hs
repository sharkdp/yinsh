{-# LANGUAGE BangPatterns #-}

-- |
-- Data structures and functions for playing the board game Yinsh.

module Yinsh where

import Control.Monad (guard)
import qualified Data.Map.Lazy as M
import Data.List (delete, foldl', sortBy)
import Data.Ord (comparing)

-- $setup
-- >>> import Data.List (sort, nub)
-- >>> import Test.QuickCheck hiding (vector)
-- >>> let boardCoords = elements coords
-- >>> instance Arbitrary Direction where arbitrary = elements directions

-- | Yinsh hex coordinates.
type YCoord = (Int, Int)

-- | The six hex directions.
data Direction = N | NE | SE | S | SW | NW
                 deriving (Eq, Enum, Bounded, Show, Read)

-- | Board element (ring or marker).
data Element = Ring Player
             | Marker Player
             deriving (Show, Eq, Read)

-- | Status of the game (required action). The two modes @WaitRemoveRun@ and
-- @WaitAddMarker@ are introduced to preserve the alternating turn structure
-- for the minmax-algorithm. The full structure is explained in the figure:
--
-- <<turn-structure.svg>>
data TurnMode = AddRing              -- ^ place a ring on a free field
              | AddMarker            -- ^ place a marker in one of your rings
              | MoveRing YCoord      -- ^ move the ring at the given position
              | RemoveRun Player     -- ^ remove (one of your) run(s).
                                     -- the parameter holds the last player who
                                     -- moved a ring
              | RemoveRing Player    -- ^ remove one of your rings
              | WaitRemoveRun Player -- ^ do nothing
              | WaitAddMarker        -- ^ do nothing
              deriving (Eq, Show, Read)

-- | Player types: black & white (or blue & green).
data Player = B | W
              deriving (Eq, Enum, Bounded, Show, Read)

-- | Efficient data structure for the board with two-way access.
-- The Map is used to get log(n) access to the element at a certain
-- coordinate while the lists are used to get direct access to the
-- coordinates of the markers and rings (which would need a reverse
-- lookup otherwise). This comes at the cost of complete redundancy.
-- Either bmap or the other four fields would be enough to reconstruct
-- the whole board.
data Board = Board { bmap :: M.Map YCoord Element
                   , ringsB :: [YCoord]
                   , ringsW :: [YCoord]
                   , markersB :: [YCoord]
                   , markersW :: [YCoord]
                   } deriving (Eq, Show, Read)

-- | Yinsh game state.
data GameState = GameState
    { activePlayer :: Player -- ^ player which has to move next
    , turnMode :: TurnMode   -- ^ required action
    , board :: Board         -- ^ current Yinsh board
    , pointsB :: Int         -- ^ number of runs / rings removed (black)
    , pointsW :: Int         -- ^ number of runs / rings removed (white)
    } deriving (Eq, Show, Read)

-- | Get all marker coordinates of one player.
markers :: Player -> Board -> [YCoord]
markers B = markersB
markers W = markersW

-- | Get all ring coordinates of one player.
rings :: Player -> Board -> [YCoord]
rings B = ringsB
rings W = ringsW

-- | Returns (Just) the element at a certain position or Nothing if the
-- coordinate is free (or invalid).
elementAt :: Board -> YCoord -> Maybe Element
elementAt b c = M.lookup c (bmap b)

-- | Returns True if the element at the given point is a marker of any color.
isMarker :: Board -> YCoord -> Bool
isMarker b c = case elementAt b c of
                   (Just (Marker _)) -> True
                   _ -> False

-- | Returns True if the element at the given point is a ring of any color.
isRing :: Board -> YCoord -> Bool
isRing b c = case elementAt b c of
                   (Just (Ring _)) -> True
                   _ -> False

-- | Returns True if a certain point on the board is free. Does not check the
-- validity of the coordinate.
freeCoord :: Board -> YCoord -> Bool
freeCoord b c = not $ M.member c (bmap b)

-- | Returns a new board with the specified element added at the given
-- coordinate.
addElement :: Board -> YCoord -> Element -> Board
addElement b c e = case e of
                       Ring B -> b { bmap = bmap'
                                   , ringsB = c : ringsB b }
                       Ring W -> b { bmap = bmap'
                                   , ringsW = c : ringsW b }
                       Marker B -> b { bmap = bmap'
                                   , markersB = c : markersB b }
                       Marker W -> b { bmap = bmap'
                                   , markersW = c : markersW b }
    where bmap' = M.insert c e (bmap b)

-- | Returns a new board with the element at the given point removed.
removeElement :: Board -> YCoord -> Board
removeElement b c = case e of
                       Ring B -> b { bmap = bmap'
                                   , ringsB = delete c (ringsB b) }
                       Ring W -> b { bmap = bmap'
                                   , ringsW = delete c (ringsW b) }
                       Marker B -> b { bmap = bmap'
                                   , markersB = delete c (markersB b) }
                       Marker W -> b { bmap = bmap'
                                   , markersW = delete c (markersW b) }
    where bmap' = M.delete c (bmap b)
          e = bmap b M.! c

-- | Returns a new board with the element at the given point replaced.
modifyElement :: Board -> YCoord -> Element -> Board
modifyElement b c = addElement (removeElement b c) c
-- TODO: this can certainly be optimizied:

-- | Yinsh board without any elements.
emptyBoard :: Board
emptyBoard = Board { bmap = M.empty
                   , ringsB = []
                   , ringsW = []
                   , markersB = []
                   , markersW = []
                   }

-- | Required runs for a win.
pointsForWin = 3
pointsForWin :: Int

-- | Similar to Enum's succ, but for cyclic data structures.
-- Wraps around to the beginning when it reaches the 'last' element.
next :: (Eq a, Enum a, Bounded a) => a -> a
next x | x == maxBound = minBound
       | otherwise     = succ x

-- | All six directions on the board.
directions :: [Direction]
directions = [minBound .. maxBound]

-- | Opposite direction (rotated by 180°).
--
-- prop> (opposite . opposite) d == d
opposite :: Direction -> Direction
opposite = next . next . next

-- | Vector to the next point on the board in a given direction.
vector :: Direction -> YCoord
vector N  = ( 0,  1)
vector NE = ( 1,  1)
vector SE = ( 1,  0)
vector S  = ( 0, -1)
vector SW = (-1, -1)
vector NW = (-1,  0)

-- | Check if the point is within the boundaries of the board.
-- All Yinsh coordinates on a hexagonal grid within a circle of radius 4.6.
validCoord :: YCoord -> Bool
validCoord (x', y') = (0.5 * sqrt 3 * x)**2 + (0.5 * x - y)**2 <= 4.6**2
    where x = fromIntegral x'
          y = fromIntegral y'

-- | All points on the board.
--
-- >>> length coords
-- 85
--
coords :: [YCoord]
coords = sortCoords [ (x, y) | x <- [-5 .. 5]
                             , y <- [-5 .. 5]
                             , validCoord (x, y) ]

-- | Sort the coords once with respect to the distance from the center
-- for a better move ordering in the game tree.
sortCoords :: [YCoord] -> [YCoord]
sortCoords = sortBy (comparing norm2)

-- | Check if two points are connected by a line.
--
-- >>> connected (3, 4) (8, 4)
-- True
--
-- prop> connected c1 c2 == connected c2 c1
--
connected :: YCoord -> YCoord -> Bool
connected (x, y) (a, b) =        x == a
                          ||     y == b
                          || x - y == a - b

-- | Vectorially add two coordinates.
add :: YCoord -> YCoord -> YCoord
add (!x1, !y1) (!x2, !y2) = (x1 + x2, y1 + y2)

-- | Vectorially subtract two coordinates.
sub :: YCoord -> YCoord -> YCoord
sub (!x1, !y1) (!x2, !y2) = (x1 - x2, y1 - y2)

-- | Squared norm.
norm2 :: YCoord -> Int
norm2 (x, y) = x * x + y * y

-- | Get a line of points from a given coordinate to the edge of the board.
ray :: YCoord -> Direction -> [YCoord]
ray s d = takeWhile validCoord $ adjacent s d

-- | All coordinates for a ring move in a given direction.
ringMovesD :: Board -> YCoord -> Direction -> [YCoord]
ringMovesD b s d = free ++ freeAfterJump
    where line = tail (ray s d)
          (free, rest) = span (freeCoord b) line
          freeAfterJump = jumpPos rest
          jumpPos [] = []
          jumpPos (c:cs) = case elementAt b c of
              (Just (Ring _)) -> []
              (Just (Marker _)) -> jumpPos cs
              Nothing -> [c]

-- | Get all valid ring moves starting from a given point.
ringMoves :: Board -> YCoord -> [YCoord]
ringMoves b start = ringMovesD b start =<< directions

-- | Check if a player has a run of five in a row.
hasRun :: Board -> Player -> Bool
hasRun b p = any (hasRunD b p) [NW, N, NE]

isMarkerOf :: Board -> Player -> YCoord -> Bool
isMarkerOf b p c = case elementAt b c of
                       Just (Marker x) -> x == p
                       _               -> False

hasRunD :: Board -> Player -> Direction -> Bool
hasRunD b p d = any middleOfRun ms
    where ms = markers p b
          middleOfRun c = all (isMarkerOf b p) surrounding
              where surrounding = left ++ right
                    left  = take 2 $ tail $ adjacent c d
                    right = take 2 $ tail $ adjacent c $ opposite d
-- TODO: this can be improved.. we are checking for every marker if it sits
-- in the middle of a run

-- | Check if a coordinate is one of five in a row.
--
-- prop> partOfRun (take 5 $ adjacent c d) c == True
partOfRun :: [YCoord] -> YCoord -> Bool
partOfRun ms start = any partOfRunD [NW, N, NE]
    where partOfRunD :: Direction -> Bool
          partOfRunD dir = length (runCoordsD ms start dir) == 5

-- | Return the coordinates of the markers making up a run.
runCoords :: [YCoord] -> YCoord -> [YCoord]
runCoords ms start = if null cs then [] else head cs
    where cs = filter ((== 5) . length) $ map (runCoordsD ms start) [NW, N, NE]

-- | Combine two lists by taking elements alternatingly. If one list is longer,
-- append the rest.
--
-- prop> zipAlternate [] l == l
-- prop> zipAlternate l [] == l
-- prop> zipAlternate l l  == (l >>= (\x -> [x, x]))
zipAlternate :: [a] -> [a] -> [a]
zipAlternate []     ys = ys
zipAlternate (x:xs) ys = x : zipAlternate ys xs

-- | Get adjacent coordinates in a given direction which could belong to a run.
--
-- prop> runCoordsD (take 7 $ adjacent c d) c d == (take 5 $ adjacent c d)
runCoordsD :: [YCoord] -> YCoord -> Direction -> [YCoord]
runCoordsD ms start dir = if start `elem` ms
                          then take 5 $ zipAlternate right left
                          else []
    where right = takeAvailable dir
          left  = tail $ takeAvailable (opposite dir)  -- use tail to avoid taking the start twice
          takeAvailable d = takeWhile (`elem` ms) $ adjacent start d

-- | Get the adjacent (including start) coordinates in a given direction.
adjacent :: YCoord -> Direction -> [YCoord]
adjacent start dir = iterate step start
    where step = add (vector dir)

-- | Get all coordinates connecting two points.
coordLine :: YCoord -> YCoord -> [YCoord]
coordLine x y = take (num - 1) . tail $ iterate (`add` step) x
    where (d1, d2) = y `sub` x
          num = max (abs d1) (abs d2)
          reduce s = round $ fromIntegral s / fromIntegral num
          step = (reduce d1, reduce d2)

-- | Flip all markers between two given coordinates.
flippedMarkers :: Board -> YCoord -> YCoord -> Board
flippedMarkers b s e = foldl' flipMaybe b (coordLine s e)
    where flipMaybe b' c = case elementAt b' c of
                               Nothing -> b'
                               (Just (Marker B)) -> modifyElement b' c (Marker W)
                               (Just (Marker W)) -> modifyElement b' c (Marker B)
                               _ -> error "trying to flip something that is not a marker (invalid ring move?)"

-- | Check whether one player has won the game.
terminalState :: GameState -> Bool
terminalState gs = pointsB gs == pointsForWin || pointsW gs == pointsForWin

-- | Get new game state after 'interacting' at a certain coordinate. Returns
-- @Nothing@ if the action leads to an invalid turn. For details, see
-- documentation of the @TurnMode@ type.
newGameState :: GameState -> YCoord -> Maybe GameState
-- TODO: the guards should be (?) unnecessary when calling this function
-- from 'gamestates'.. (for AI-only matches). unless the AI tries to cheat..
newGameState gs cc =
    case turnMode gs of
        AddRing -> do
            guard (freeCoord board' cc)
            Just gs { activePlayer = nextPlayer
                    , turnMode = if numRings < 9 then AddRing else AddMarker
                    , board = addElement board' cc (Ring activePlayer')
                    }
            where numRings = length (ringsB board') + length (ringsW board')
        AddMarker -> do
            guard (cc `elem` rings activePlayer' board')
            Just gs { turnMode = MoveRing cc
                    , board = addElement removedRing cc (Marker activePlayer')
                    }
        (MoveRing start) -> do
            guard (cc `elem` ringMoves board' start)
            Just gs { activePlayer = nextPlayer
                    , turnMode = nextTurnMode
                    , board = addElement flippedBoard cc (Ring activePlayer')
                    }
            where nextTurnMode | hasRun flippedBoard activePlayer' = WaitRemoveRun activePlayer'
                               | hasRun flippedBoard nextPlayer    = RemoveRun activePlayer'
                               | otherwise                         = AddMarker
                  flippedBoard = flippedMarkers board' start cc
        (RemoveRun lastRingMove) -> do
            guard (partOfRun playerMarkers cc)
            Just gs { turnMode = RemoveRing lastRingMove
                    , board = removedRun
                    }
        (RemoveRing lastRingMove) -> do
            guard (cc `elem` rings activePlayer' board')
            Just gs { activePlayer = nextPlayer
                    , turnMode = nextTurnMode
                    , board = removedRing
                    , pointsB = if activePlayer' == B then pointsB gs + 1 else pointsB gs
                    , pointsW = if activePlayer' == W then pointsW gs + 1 else pointsW gs
                    }
            where nextTurnMode | hasRun removedRing activePlayer'
                                   = WaitRemoveRun lastRingMove -- player has a second run
                               | hasRun removedRing nextPlayer
                                   = RemoveRun lastRingMove     -- opponent also has a run
                               | otherwise
                                   = if lastRingMove == activePlayer'
                                     then AddMarker
                                     else WaitAddMarker
        (WaitRemoveRun lastRingMove) ->
            Just gs { activePlayer = nextPlayer
                    , turnMode = RemoveRun lastRingMove
                    }
        WaitAddMarker ->
            Just gs { activePlayer = nextPlayer
                    , turnMode = AddMarker
                    }
    where activePlayer' = activePlayer gs
          nextPlayer    = next activePlayer'
          removedRing   = removeElement board' cc
          removedRun    = foldl' removeElement board' (runCoords playerMarkers cc)
          board'        = board gs
          playerMarkers = markers activePlayer' board'

initialGameState :: GameState
initialGameState = GameState { activePlayer = B
                             , turnMode = AddRing
                             , board = emptyBoard
                             , pointsW = 0
                             , pointsB = 0
                             }

-- Testing stuff

testBoard :: Board
testBoard = foldl' (\b (c, e) -> addElement b c e) emptyBoard
                [ ((3 - 6, 4 - 6), Ring B)
                , ((4 - 6, 9 - 6), Ring B)
                , ((7 - 6, 9 - 6), Ring B)
                , ((8 - 6, 9 - 6), Ring B)
                , ((7 - 6, 10 - 6), Ring B)
                , ((8 - 6, 7 - 6), Ring W)
                , ((6 - 6, 3 - 6), Ring W)
                , ((4 - 6, 8 - 6), Ring W)
                , ((4 - 6, 2 - 6), Ring W)
                , ((2 - 6, 5 - 6), Ring W)
                , ((6 - 6, 4 - 6), Marker W)
                , ((6 - 6, 5 - 6), Marker W)
                , ((6 - 6, 7 - 6), Marker W)
                , ((5 - 6, 5 - 6), Marker W)
                , ((4 - 6, 5 - 6), Marker W)
                , ((3 - 6, 5 - 6), Marker W)
                , ((6 - 6, 6 - 6), Marker B)]

testGameState = GameState { activePlayer = B
                          , turnMode = AddMarker
                          , board = testBoard
                          , pointsW = 0
                          , pointsB = 0
                          }

testGameStateW = GameState { activePlayer = W
                          , turnMode = AddMarker
                          , board = testBoard
                          , pointsW = 0
                          , pointsB = 0
                          }

