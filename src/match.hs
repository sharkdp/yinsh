import System.IO

import Yinsh
import AI
import Floyd
import RandomAI

makeFloyd :: Int -> GameState -> Floyd
makeFloyd pl gs = Floyd { floydGS = gs, floydPL = pl }

makeRandomAI :: Int -> GameState -> RandomAI
makeRandomAI pl gs = RandomAI { raiGS = gs, raiPL = pl }

aiPlay :: Player -> GameState -> GameState
aiPlay W = aiTurn . makeFloyd 4
aiPlay B = aiTurn . makeFloyd 3

handleTurn :: Int -> GameState -> IO ()
handleTurn turn gs = do
    putStrLn $ "Turn #" ++ show turn
    print gs'
    if terminalState gs'
    then do
        let winningPlayer = if pointsW gs' == pointsForWin then W else B
        putStrLn $ "Player " ++ show winningPlayer ++ " won the game after " ++ show turn ++ " turns"
    else
        handleTurn (turn + 1) gs'
    where player' = activePlayer gs
          gs' = aiPlay player' gs

handleTurn' :: Int -> GameState -> IO ()
handleTurn' turn gs =
    if terminalState gs'
    then do
        putStr $ show gs'
        putStr "]"
    else do
        putStr $ show gs'
        putStr ", "
        hFlush stdout
        handleTurn' (turn + 1) gs'
    where player' = activePlayer gs
          gs' = aiPlay player' gs

main :: IO ()
main = do
    let initPlayer = W
    let hsOutput = False
    let initGS = initialGameState { activePlayer = initPlayer }

    if hsOutput
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
