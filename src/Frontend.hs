module Main where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.IORef
import Control.Monad (when, forM_)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Haste (elemById, onEvent, Event(..), setTimeout)
import Haste.Graphics.Canvas

import Yinsh
import AI
import Floyd

-- testing:
-- import AIHistory

-- | Current state of the user interface
data DisplayState = WaitUser | WaitAI | ViewBoard | ViewHistory Int
                    deriving (Show, Eq)

-- | Pixel coordinate on the screen
type ScreenCoord = (Int, Int)

-- Color theme (http://www.colourlovers.com/palette/15/tech_light)
green  = RGB  209 231  81
blue   = RGB   38 173 228
white  = RGB  255 255 255
hl     = RGBA 255   0   0 0.5
black  = RGB    0   0   0

-- Dimensions
spacing         = 60 :: Double
markerWidth     = 20 :: Double
ringInnerRadius = 22 :: Double
ringWidth       = 6 :: Double
originX         = 600 / 2 :: Double -- Half the canvas size
originY         = 630 / 2 :: Double

-- Keyboard codes
keyLeft = 37
keyRight = 39

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

-- | Get the board coordinate which is closest to the given screen coordinate.
closestCoord :: ScreenCoord -> YCoord
closestCoord (xi, yi) = coords !! snd lsort
    where lind = zipWith (\p i -> (dist p, i)) points [0..]
          lsort = minimumBy (comparing fst) lind
          dist (x', y') = (x - x')^2 + (y - y')^2
          x = fromIntegral xi
          y = fromIntegral yi

-- | Marker and ring color for each player.
playerColor :: Player -> Color
playerColor B = blue
playerColor W = green

-- | Update the game state after interacting at a certain coordinate. If this
-- is an illegal action, @newGameState@ returns @Nothing@ and the state is left
-- unchanged.
updateState :: GameState  -- ^ old state
            -> YCoord     -- ^ clicked coordinate
            -> GameState  -- ^ new state
updateState gs cc = fromMaybe gs (newGameState gs cc)

-- | Specify the AI player for the frontend
frontendAI :: AIFunction
frontendAI = aiFloyd 3 mhNumber rhZero

-- | Get new game state after AI turn. This also resolves @WaitRemoveRun@ and
-- @WaitAddMarker@ turns for the *human* player.
aiTurn' :: GameState -> GameState
aiTurn' gs = let gs' = frontendAI gs in
                 case turnMode gs' of
                     (WaitRemoveRun _) -> frontendAI $ fromJust $ newGameState gs' (0, 0)
                     WaitAddMarker     -> frontendAI $ fromJust $ newGameState gs' (0, 0)
                     _                 -> gs'

-- Monadic code

pSetPlayerColor :: Player -> Picture ()
pSetPlayerColor = setFillColor . playerColor

-- | Draw ring.
pRing :: Player     -- ^ Player (for color)
      -> Bool       -- ^ Draw the grid lines inside the ring?
      -> Picture ()
pRing p drawCross = do
    -- Draw filled circle in player color
    pSetPlayerColor p
    fill circL
    stroke circL

    -- Draw white inner circle
    setFillColor white
    fill circS
    stroke circS

    -- Redraw the grid lines inside
    when drawCross $ pCross ringInnerRadius

    where circL = circle (0, 0) (ringInnerRadius + ringWidth)
          circS = circle (0, 0) ringInnerRadius

-- | Draw marker.
pMarker :: Player -> Picture ()
pMarker p = do
    pSetPlayerColor p
    fill circ
    stroke circ
        where circ = circle (0, 0) markerWidth

-- | Draw board element (ring or marker)
pElement :: Element -> YCoord -> Picture ()
pElement (Ring p)   c = translateC c $ pRing p True
pElement (Marker p) c = translateC c $ pMarker p

-- | Draw three crossing grid lines at current position
pCross :: Double -- ^ Length of grid lines
       -> Picture ()
pCross len = do
    l
    rotate (2 * pi / 3) l
    rotate (4 * pi / 3) l
        where l = stroke $ line (0, -len) (0, len)

-- | Highlight a marker on the board with a ring around it.
pHighlightRing = fill $ circle (0, 0) (markerWidth + 2)

-- | Highlight markers making up a run.
pHighlight :: Board -> Player -> Picture ()
pHighlight b p = mapM_ (`translateC` pHighlightRing) mcH
    where mc  = markers p b
          mcH = filter (partOfRun mc) mc

-- | Draw small black dot at current position to indicate a valid ring move.
pDot :: Picture ()
pDot = do setFillColor black
          fill $ circle (0, 0) 5

-- | Draw the whole board including the board elements.
pBoard :: Board -> Picture ()
pBoard b = do
    -- Draw grid
    sequence_ $ mapM translate points (pCross (0.5 * spacing))

    -- Draw board elements
    forM_ [B, W] $ \p -> do
        mapM_ (pElement (Marker p)) $ markers p b
        mapM_ (pElement (Ring p)) $ rings p b

-- | Draw elements which are specific to the current turn mode.
pAction :: Board    -- ^ Current board
        -> TurnMode -- ^ turn mode
        -> YCoord   -- ^ coordinate closest to the mouse
        -> Player   -- ^ active player
        -> Picture ()
pAction b AddRing          mc p = when (freeCoord b mc) $ pElement (Ring p) mc
pAction b AddMarker        mc p = when (mc `elem` rings p b) $ pElement (Marker p) mc
pAction b (MoveRing start) mc p = do
    let allowed = ringMoves b start
    mapM_ (`translateC` pDot) allowed
    when (mc `elem` allowed) $ pElement (Ring p) mc
pAction b (RemoveRun _)    mc p = do
    let runC = runCoords (markers p b) mc
    setFillColor hl
    mapM_ (`translateC` pHighlightRing) runC
pAction _ _ _ _                 = return ()

-- | Draw rings which are already removed from the board
pRings :: Player -> Int -> Picture ()
pRings p rw =
    mapM_ ringAt cs
    where cs = take rw $ iterate (diff p) (initial p)
          initial B = screenPoint (5, -2)
          initial W = screenPoint (-5, 2)
          diff B (x, y) = (x - 20, y)
          diff W (x, y) = (x + 20, y)
          ringAt point = translate point $ pRing p False

-- | Render all screen elements.
pDisplay :: GameState
         -> Maybe YCoord  -- ^ Coordinate close to mouse cursor
         -> Picture ()
pDisplay gs mmc = do
    when (terminalState gs) $
        font "13pt 'Lato', sans-serif" $
            text (420, 20) message

    pBoard (board gs)

    pRings B (pointsB gs)
    pRings W (pointsW gs)

    -- Draw thick borders for markers which are part of a run
    case turnMode gs of
        (RemoveRun _) -> mapM_ (pHighlight (board gs)) [B, W]
        _             -> return ()

    -- Render screen action
    when (isJust mmc && not (terminalState gs)) $ do
        let (Just mc) = mmc
        -- TODO: just debugging:
        font "13pt 'Lato', sans-serif" $
            text (550, 620) $ show mc

        pAction (board gs) (turnMode gs) mc (activePlayer gs)

        when (activePlayer gs == W) $
            font "13pt 'Lato', sans-serif" $
                text (420, 20) "Floyd is thinking ..."

    where message | pointsB gs == pointsForWin = "You win!"
                  | otherwise                  = "Floyd wins!"

-- | Draw on canvas.
renderCanvas :: Canvas -> GameState -> Maybe ScreenCoord -> IO ()
renderCanvas can gs mAction = render can $ pDisplay gs (closestCoord `fmap` mAction)

-- | Register IO events.
main :: IO ()
main = do
    Just can <- getCanvasById "canvas"
    Just ce  <- elemById "canvas"

    -- let initGS = initialGameState
    let initGS = testGameState
    let initBoard = board initGS

    -- 'ioState' holds a chronological list of game states and the display
    -- state.
    let initHistory = [initGS]
    ioState <- newIORef (initHistory, WaitUser)

    -- draw initial board
    render can (pBoard initBoard)

    _ <- ce `onEvent` OnMouseMove $ \point -> do
        (gs:_, ds) <- readIORef ioState
        when (ds == WaitUser) $
            renderCanvas can gs (Just point)

    _ <- ce `onEvent` OnKeyDown $ \key -> do
        when (key == keyLeft) $ do
            (gslist, ds) <- readIORef ioState
            let numGS = length gslist
            when (numGS > 1) $
                if ds == WaitUser || ds == ViewBoard then do
                    writeIORef ioState (gslist, ViewHistory 1)
                    renderCanvas can (gslist !! 1) Nothing
                else
                    case ds of
                        ViewHistory h ->
                            when (h + 1 < numGS) $ do
                                writeIORef ioState (gslist, ViewHistory (h + 1))
                                let gs = gslist !! (h + 1)
                                renderCanvas can gs Nothing
                                putStrLn $ "DEBUG: let gs = " ++ show gs
                        _ -> return ()
        when (key == keyRight) $ do
            (gslist, ds) <- readIORef ioState
            case ds of
                ViewHistory h -> do
                    let newDS = if h == 1 then WaitUser else ViewHistory (h - 1)
                    writeIORef ioState (gslist, newDS)
                    let gs = gslist !! (h - 1)
                    renderCanvas can gs Nothing
                    putStrLn $ "DEBUG: let gs = " ++ show gs
                _ -> return ()

    _ <- ce `onEvent` OnClick $ \_ point -> do
        (oldGS:gslist, ds) <- readIORef ioState
        when (ds == WaitUser) $ do
            let gs = updateState oldGS (closestCoord point)
            let gameover = terminalState gs
            renderCanvas can gs (Just point)

            putStrLn $ "DEBUG: let gs = " ++ show gs

            if activePlayer gs == W && not gameover
            then do -- AI turn
                writeIORef ioState (gs:oldGS:gslist, WaitAI)
                setTimeout 0 $ do
                    let gs' = aiTurn' gs
                    let gameover' = terminalState gs'
                    let ds' = if gameover' then ViewBoard else WaitUser
                    renderCanvas can gs' (Just point)
                    writeIORef ioState (gs':gs:oldGS:gslist, ds')
            else do -- users turn or game over
                let ds' = if gameover then ViewBoard else WaitUser
                writeIORef ioState (gs:oldGS:gslist, ds')

    return ()
