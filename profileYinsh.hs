import Yinsh
import Floyd

main = do
    putStrLn $ "Running profile at " ++ show plies ++ "-ply"
    print $ aiRes 5 testGameStateW
