import System.IO
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq

import Yinsh
import AI
import Floyd
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
-- Output: A list of all matches with the winning player
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
aiPlay W = aiFloyd 3 mhNumber rhZero
aiPlay B = aiFloyd 3 mhNumber rhConnected

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

ioTournament :: IO ()
ioTournament = do
    let aiList = [ ("Floyd 3-ply C", aiFloyd 3 mhNumber rhConnected)
                 , ("Floyd 3-ply M", aiFloyd 3 mhNumber rhRingMoves)
                 , ("Floyd 4-ply C", aiFloyd 4 mhNumber rhConnected)
                 -- , ("Floyd 4-ply M", aiFloyd 4 mhNumber rhRingMoves)
                 -- , ("Floyd 5-ply C", aiFloyd 5 mhNumber rhConnected)
                 , ("Rai Charles  ", aiRaiCharles 1)
                 ]
        res = aiTournament aiList
        out = unlines $ map matchLine res
        matchLine (nW, nB, win, m) =
            fW nW ++ "  vs.  " ++ fB nB ++ "    (" ++ show m ++ " moves)"
            where (fW, fB) = if win == W then (sWin, sLose) else (sLose, sWin)
                  sWin str =  "* \x1b[01m" ++ str ++ "\x1b[0m"
                  sLose str = "  " ++ str

    putStrLn $ "Playing Yinsh Tournament with " ++ show (length aiList) ++ " AI players"
    putStrLn ""
    putStrLn out

main :: IO ()
-- main = ioTournament
main = do
    args <- getArgs
    if not (null args) && head args == "-single"
    then
        singleGame True
    else
        ioTournament

