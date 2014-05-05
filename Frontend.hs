module Main where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.IORef
import Control.Monad (when)
import Data.Maybe (fromJust, fromMaybe)
import Haste hiding (next)
import Haste.Graphics.Canvas

import Yinsh
import Floyd

data DisplayState = BoardOnly GameState -- TODO: do we need this state?
                  | WaitTurn GameState

-- Color theme
-- http://www.colourlovers.com/palette/15/tech_light
green  = RGB  209 231  81
blue   = RGB   38 173 228
white  = RGB  255 255 255
hl     = RGBA 255   0   0 0.5

-- Dimensions
spacing         = 60 :: Double
markerWidth     = 20 :: Double
ringInnerRadius = 22 :: Double
ringWidth       = 6 :: Double
originX         = -15 :: Double
originY         = 495 :: Double

-- | Translate hex coordinates to screen coordinates
screenPoint :: YCoord -> Point
screenPoint (ya, yb) = (0.5 * sqrt 3 * x' + originX, - y' + 0.5 * x' + originY)
    where x' = spacing * fromIntegral ya
          y' = spacing * fromIntegral yb

-- | All grid points as screen coordinates
points :: [Point]
points = map screenPoint coords

-- | Translate by hex coordinate
translateC :: YCoord -> Picture () -> Picture ()
translateC = translate . screenPoint

playerColor :: Player -> Color
playerColor B = blue
playerColor W = green

setPlayerColor :: Player -> Picture ()
setPlayerColor = setFillColor . playerColor

pRing :: Player -> Bool -> Picture ()
pRing p drawCross = do
    setPlayerColor p
    fill circL
    stroke circL
    setFillColor white
    fill circS
    stroke circS
    when drawCross $ pCross ringInnerRadius
        where circL = circle (0, 0) (ringInnerRadius + ringWidth)
              circS = circle (0, 0) ringInnerRadius

pMarker :: Player -> Picture ()
pMarker p = do
    setPlayerColor p
    fill circ
    stroke circ
        where circ = circle (0, 0) markerWidth

pElement :: Element -> Picture ()
pElement (Ring p c)   = translateC c $ pRing p True
pElement (Marker p c) = translateC c $ pMarker p

pCross :: Double -> Picture ()
pCross len = do
    l
    rotate (2 * pi / 3) l
    rotate (4 * pi / 3) l
        where l = stroke $ line (0, -len) (0, len)

pHighlightRing :: Picture ()
pHighlightRing = fill $ circle (0, 0) (markerWidth + 2)

pHighlight :: Board -> Player -> Picture ()
pHighlight b p = do
    let mc  = markers p b
    let mcH = filter (partOfRun mc) mc
    mapM_ (`translateC` pHighlightRing) mcH

pDot :: Picture ()
pDot = do
    setFillColor $ RGB 0 0 0
    fill $ circle (0, 0) 5

pBoard :: Board -> Picture ()
pBoard b = do
    -- Draw grid
    sequence_ $ mapM translate points (pCross (0.5 * spacing))

    -- Draw markers
    mapM_ pElement b

    -- sequence_ $ mapM (translate . screenPoint) (reachable (3, 6)) pDot
    -- Testing
    -- mapM_ (`translateC` pDot) $ fiveAdjacent (6, 6) NW

pAction :: Board -> TurnMode -> YCoord -> Player -> Picture ()
pAction b AddMarker mc p        = when (mc `elem` rings p b) $ pElement (Marker p mc)
pAction b AddRing mc p          = when (freeCoord b mc) $ pElement (Ring p mc)
pAction b (MoveRing start) mc p = do
    let allowed = validRingMoves b start
    mapM_ (`translateC` pDot) allowed
    when (mc `elem` allowed) $ pElement (Ring p mc)
pAction b RemoveRun mc p        = do
    let runC = runCoords (markers p b) mc
    setFillColor hl
    mapM_ (`translateC` pHighlightRing) runC
pAction _ RemoveRing _ _        = return ()
pAction _ PseudoTurn _ _        = return ()

pRings :: Player -> Int -> Picture ()
pRings p rw =
    mapM_ ringAt cs
    where cs = take rw $ iterate (diff p) (initial p)
          initial B = screenPoint (11, 4)
          initial W = screenPoint (1, 8)
          diff B (x, y) = (x - 20, y)
          diff W (x, y) = (x + 20, y)
          ringAt point = translate point $ pRing p False

-- | Render everything that is seen on the screen
pDisplay :: DisplayState
         -> YCoord         -- ^ Coordinate close to mouse cursor
         -> Picture ()
pDisplay (BoardOnly gs) _ = pBoard (board gs)
pDisplay (WaitTurn gs) mc = do
    pBoard (board gs)

    pRings B (ringsB gs)
    pRings W (ringsW gs)

    pAction (board gs) (turnMode gs) mc (activePlayer gs)

    -- Draw thick borders for markers which are part of a run
    when (turnMode gs == RemoveRun) $
        mapM_ (pHighlight (board gs)) [B, W]

    when (activePlayer gs == W) $
        font "15pt 'Lato', sans-serif" $
            text (420, 20) "Floyd is thinking ..."

-- | Get the board coordinate which is closest to the given screen
-- coordinate point
--
-- prop> closestCoord p == (closestCoord . screenPoint . closestCoord) p
closestCoord :: Point -> YCoord
closestCoord (x, y) = coords !! snd lsort
    where lind = zipWith (\p i -> (dist p, i)) points [0..]
          lsort = minimumBy (comparing fst) lind
          dist (x', y') = (x - x')^2 + (y - y')^2

renderCanvas :: Canvas -> DisplayState -> (Int, Int) -> IO ()
renderCanvas can ds point = render can $ pDisplay ds (coordFromXY point)

coordFromXY :: (Int, Int) -> YCoord
coordFromXY (x, y) = closestCoord (fromIntegral x, fromIntegral y)

newDisplayState :: DisplayState  -- ^ old state
                -> YCoord        -- ^ clicked coordinate
                -> DisplayState  -- ^ new state
newDisplayState (WaitTurn gs) cc = WaitTurn $ fromMaybe gs (newGameState gs cc)
newDisplayState (BoardOnly gs) _ = BoardOnly gs

initialDisplayState = WaitTurn initialGameState
testDisplayState = WaitTurn testGameState -- TODO: just for testing

-- Resolve pseudo turns for the human player automatically
aiTurn' :: GameState -> GameState
aiTurn' gs = let gs' = aiTurn gs in
                 if turnMode gs' == PseudoTurn
                 then aiTurn (fromJust $ newGameState gs' (0, 0))
                 else gs'

main :: IO ()
main = do
    Just can <- getCanvasById "canvas"
    Just ce  <- elemById "canvas"

    ioDS <- newIORef initialDisplayState
    -- ioDS <- newIORef testDisplayState

    -- draw initial board
    render can (pBoard []) -- TODO: use this line instead of next
    -- render can (pBoard testBoard)

    ce `onEvent` OnMouseMove $ \point -> do
        -- putStrLn $ "Coord: " ++ show (coordFromXY point)
        ds <- readIORef ioDS
        renderCanvas can ds point

    ce `onEvent` OnClick $ \_ point -> do
        old <- readIORef ioDS
        let newDS = newDisplayState old (coordFromXY point)
        renderCanvas can newDS point

        let WaitTurn gs = newDS
        if activePlayer gs == W
        then
            -- putStrLn "AI move"
            setTimeout 0 $ do
                let gs' = aiTurn' gs
                let newDS' = WaitTurn gs'
                renderCanvas can newDS' point
                writeIORef ioDS newDS'
        else
            writeIORef ioDS newDS

    return ()
