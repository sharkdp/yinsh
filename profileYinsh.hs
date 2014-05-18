import Yinsh
import Floyd

profPlies = 5

main = do
    putStrLn $ "Running profile at " ++ show profPlies ++ "-ply"
    print $ aiRes profPlies testGameStateW
