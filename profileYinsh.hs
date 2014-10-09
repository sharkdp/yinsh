import Yinsh
import Floyd

profPlies = 4

main = do
    putStrLn $ "Running profile at " ++ show profPlies ++ "-ply"
    -- print $ aiRes profPlies testGameStateW
    print $ aiRes profPlies $ initialGameState { activePlayer = W }
