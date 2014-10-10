import Yinsh
import Floyd
import System.IO

playW :: GameState -> GameState
playW = aiTurn 3

playB :: GameState -> GameState
playB = aiTurn 3

handleTurn :: Int -> GameState -> IO ()
handleTurn turn gs = do
    let gs' = fn' gs
    putStrLn $ "Turn #" ++ show turn
    print gs'
    if terminalState gs'
    then do
        let winningPlayer = if pointsW gs' == pointsForWin then W else B
        putStrLn $ "Player " ++ show winningPlayer ++ " won the game after " ++ show turn ++ " turns"
    else
        handleTurn (turn + 1) gs'
    where player' = activePlayer gs
          fn' = if player' == W then playW else playB

handleTurn' :: Int -> GameState -> IO ()
handleTurn' turn gs = do
    let gs' = fn' gs
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
          fn' = if player' == W then playW else playB

main :: IO ()
main = do
    let initPlayer = W
    let hsOutput = True
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
