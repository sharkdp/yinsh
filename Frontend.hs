module Main where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.IORef
import Control.Monad (when, forM_, unless)
import Data.Maybe (fromJust, fromMaybe)
import Haste hiding (next)
import Haste.Graphics.Canvas

import Yinsh
import Floyd

-- | Current state of the user interface
data DisplayState = WaitUser | WaitAI | ViewBoard
                    deriving (Show, Eq)
-- TODO: ViewBoard and WaitAI are somehow the same up to now?

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

pElement :: Element -> YCoord -> Picture ()
pElement (Ring p)   c = translateC c $ pRing p True
pElement (Marker p) c = translateC c $ pMarker p

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

    -- Draw board elements
    forM_ [B, W] $ \p -> do
        mapM_ (pElement (Marker p)) $ markers p b
        mapM_ (pElement (Ring p)) $ rings p b

pAction :: Board -> TurnMode -> YCoord -> Player -> Picture ()
pAction b AddMarker mc p        = when (mc `elem` rings p b) $ pElement (Marker p) mc
pAction b AddRing mc p          = when (freeCoord b mc) $ pElement (Ring p) mc
pAction b (MoveRing start) mc p = do
    let allowed = validRingMoves b start
    mapM_ (`translateC` pDot) allowed
    when (mc `elem` allowed) $ pElement (Ring p) mc
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

-- | Render everything static on the screen
pDisplay :: GameState
         -> Picture ()
pDisplay gs =
    if terminalState gs
    then
        font "25pt 'Lato', sans-serif" $
            text (220, 200) message -- TODO: center this properly
    else do
        pBoard (board gs)

        pRings B (pointsB gs)
        pRings W (pointsW gs)

        -- Draw thick borders for markers which are part of a run
        when (turnMode gs == RemoveRun) $
            mapM_ (pHighlight (board gs)) [B, W]
    where message | pointsB gs == pointsForWin = "You win!"
                  | otherwise                  = "Floyd wins!"

pDisplayAction :: GameState
               -> YCoord         -- ^ Coordinate close to mouse cursor
               -> Picture ()
pDisplayAction gs mc = do
    pDisplay gs
    -- TODO: remove this duplication (same in pDisplay)
    unless (terminalState gs) $ do
        pAction (board gs) (turnMode gs) mc (activePlayer gs)

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

renderCanvas :: Canvas -> GameState -> IO ()
renderCanvas can ds = render can $ pDisplay ds

-- TODO: this structure (having renderCanvas and renderCanvasAction as well
-- as pDisplay and pDisplayAction) is not really nice...
renderCanvasAction :: Canvas -> GameState -> (Int, Int) -> IO ()
renderCanvasAction can ds point = render can $ pDisplayAction ds (coordFromXY point)

coordFromXY :: (Int, Int) -> YCoord
coordFromXY (x, y) = closestCoord (fromIntegral x, fromIntegral y)

updateState :: GameState  -- ^ old state
            -> YCoord     -- ^ clicked coordinate
            -> GameState  -- ^ new state
updateState gs cc = fromMaybe gs (newGameState gs cc)

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

    let initGS = initialGameState
    -- let initGS = testGameState
    let initBoard = board initGS

    -- 'ioState' holds a chronological list of game states and the display
    -- state.
    ioState <- newIORef ([initGS], WaitUser)

    -- draw initial board
    render can (pBoard initBoard)

    ce `onEvent` OnMouseMove $ \point -> do
        (gs:_, ds) <- readIORef ioState
        when (ds == WaitUser) $
            renderCanvasAction can gs point

    ce `onEvent` OnKeyDown $ \key ->
        when (key == 32) $ do -- Pressed 'space'
            (gslist, ds) <- readIORef ioState
            when (ds == WaitUser && length gslist > 1) $ do
                writeIORef ioState (gslist, ViewBoard)
                renderCanvas can (gslist !! 1)

    ce `onEvent` OnKeyUp $ \key ->
        when (key == 32) $ do -- Released 'space'
            (gslist, ds) <- readIORef ioState
            when (ds == ViewBoard) $ do
                writeIORef ioState (gslist, WaitUser)
                renderCanvas can (head gslist)

    ce `onEvent` OnClick $ \_ point -> do
        (oldGS:gslist, ds) <- readIORef ioState
        when (ds == WaitUser) $ do
            let gs = updateState oldGS (coordFromXY point)
            let gameover = terminalState gs
            renderCanvasAction can gs point

            putStrLn $ "DEBUG: gs = " ++ show gs

            if activePlayer gs == W && not gameover
            then do
                writeIORef ioState (gs:oldGS:gslist, WaitAI)
                setTimeout 0 $ do
                    let gs' = aiTurn' gs
                    let gameover' = terminalState gs'
                    let ds' = if gameover' then ViewBoard else WaitUser
                    renderCanvasAction can gs' point
                    writeIORef ioState (gs':gs:oldGS:gslist, ds')
            else do
                let ds' = if gameover then ViewBoard else WaitUser
                writeIORef ioState (gs:oldGS:gslist, ds')

    return ()
