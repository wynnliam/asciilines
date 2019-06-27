import System.Environment

printArgs :: (Show a) => [a] -> IO ()
printArgs [] = putStrLn ""
printArgs x = putStrLn (show x)

main = getArgs >>= printArgs
