import System.IO
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Maybe
import Data.List

import Yinsh
import AI
import Floyd
import Pink
import RaiCharles

-- | Match between two AI players
aiMatch :: AIFunction     -- ^ AI playing white
        -> AIFunction     -- ^ AI playing black
        -> (Player, Int)  -- ^ (winner, total number of moves)
aiMatch aiW aiB = turn 1 initGS
    where initGS  = initialGameState { activePlayer = W }
          turn :: Int -> GameState -> (Player, Int)
          turn t gs = let player = activePlayer gs
                          aiFunc W = aiW
                          aiFunc B = aiB
                          gs' = aiFunc player gs
                      in
                          if terminalState gs'
                          then
                              (if pointsW gs' >= pointsForWin then W else B, t)
                          else
                              turn (t + 1) gs'

-- This is needed to deeply evaluate the results of the AI matches
instance NFData Player where rnf x = x `seq` ()

pmap :: (NFData  b) => (a -> b) -> [a] -> [b]
pmap = parMap rdeepseq

-- | A tournament between different AIs, with matches played in parallel.
--
-- Input: A list of the names and AI functions
--
-- Output: A list of all matches with the winning player and number of turns
aiTournament :: [(String, AIFunction)] -> [(String, String, Player, Int)]
aiTournament aiList = pmap resolve matches
    where matches = [(nW, nB, fW, fB) | (nW, fW) <- aiList
                                      , (nB, fB) <- aiList
                                      , nW /= nB ]
          resolve (nW, nB, fW, fB) = let (w, m) = aiMatch fW fB in (nW, nB, w, m)

handleTurn :: Int -> GameState -> IO ()
handleTurn turn gs = do
    let player' = activePlayer gs
    let gs' = aiPlay player' gs

    putStrLn $ "Turn #" ++ show turn
    print gs'
    if terminalState gs'
    then do
        let winningPlayer = if pointsW gs' == pointsForWin then W else B
        putStrLn $ "Player " ++ show winningPlayer ++ " won the game after " ++ show turn ++ " turns"
    else
        handleTurn (turn + 1) gs'

handleTurn' :: Int -> GameState -> IO ()
handleTurn' turn gs = do
    let player' = activePlayer gs
    let gs' = aiPlay player' gs
    putStr $ show gs'
    if terminalState gs'
    then
        putStr "]"
    else do
        putStr ", "
        hFlush stdout
        handleTurn' (turn + 1) gs'

aiPlay :: Player -> AIFunction
aiPlay W = aiFloyd 4 mhNumber rhRingMoves
aiPlay B = aiFloyd 4 mhNumber rhConnected

singleGame :: Bool -> IO ()
singleGame haskellOutput = do
    let initGS = initialGameState { activePlayer = W }

    if haskellOutput
    then do
        putStrLn "module AIHistory where"
        putStrLn ""
        putStrLn "import Data.Map (fromList)"
        putStrLn "import Yinsh"
        putStrLn ""
        putStr "initHistory = ["

        handleTurn' 1 initGS
    else
        handleTurn 1 initGS

rhCMandC = rhCombined [(1, rhControlledMarkers), (1, rhConnected)]
rh2CMandC = rhCombined [(1, rhControlledMarkers), (1, rhConnected)]

ioTournament :: IO ()
ioTournament = do
    let aiList = [-- ("Pink  3-ply C", aiPink  3 mhNumber rhConnected)
                 --  ("Pink  3-ply M", aiPink  3 mhNumber rhRingMoves)
                   ("Floyd 3-ply C", aiFloyd 3 mhNumber rhConnected)
                 , ("Floyd 3-ply M", aiFloyd 3 mhNumber rhRingMoves)
                 , ("Floyd 3-ply CM", aiFloyd 3 mhNumber rhControlledMarkers)
                 -- , ("Floyd 3-ply CM+C", aiFloyd 3 mhNumber rhCMandC)
                 -- , ("Floyd 3-ply 2CM+C", aiFloyd 3 mhNumber rh2CMandC)
                 , ("Floyd 4-ply C", aiFloyd 4 mhNumber rhConnected)
                 , ("Floyd 4-ply M", aiFloyd 4 mhNumber rhRingMoves)
                 , ("Floyd 4-ply CM", aiFloyd 4 mhNumber rhControlledMarkers)
                 , ("Rai Charles  ", aiRaiCharles 1)
                 ]
        res = aiTournament aiList
        out = unlines $ map matchLine res
        matchLine (nW, nB, win, m) =
            fW nW ++ "  vs.  " ++ fB nB ++ "    (" ++ show m ++ " moves)"
            where (fW, fB) = if win == W then (sWin, sLose) else (sLose, sWin)
                  sWin str =  "* \x1b[01m" ++ str ++ "\x1b[0m"
                  sLose str = "  " ++ str

    let nPlayers = length aiList
    putStrLn $ "Playing Yinsh Tournament with " ++ show nPlayers ++ " AI players"
    putStrLn ""
    putStrLn out

    writeFile "match-results.html" $
        resTable (map fst aiList) (map (\(_, _, w, t) -> (w, t)) res)

colors = [ "e51c23"
         , "9c27b0"
         , "3f51b5"
         , "03a9f4"
         , "009688"
         , "8bc34a"
         , "ffeb3b"
         , "ff9800"
         , "795548"
         , "607d8b"
         ]

resTable :: [String] -> [(Player, Int)] -> String
resTable names res = "<table border='1'>\n" ++ firstRow ++ rest ++ "</table>"
    where nameCells = zipWith nameCell names colors
          nameCell n c = "<td style=\"width: 100px; height: 100px; background-color: #" ++ c ++ "\">" ++ n ++ "</td>"
          firstRow = htmlRow ("<td></td>" : nameCells)
          ns = [0 .. (length names - 1)]
          rest = ns >>= rowNr
          rowNr k = htmlRow ((nameCells !! k) : content k)
          content k = map resultCell (indTable !! k)

          indices = [(r, c) | r <- ns , c <- ns , r /= c]
          resultCell (x, y) | x == y =    "<td></td>"
                            | otherwise = winnerCell (x, y)
          winnerCell (x, y) = nameCells !! winnerInd (x, y) (fromJust $ elemIndex (x, y) indices)
          winnerInd (x, y) ind = case fst (res !! ind) of
                              W -> x
                              B -> y
          indTable = [[(r,c) | c <- ns] | r <- ns]
          htmlRow cols = "<tr>" ++ concat cols ++ "</tr>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-s"] -> singleGame False
        ["-h"] -> singleGame True
        _      -> ioTournament

