import System.Environment
import System.IO
import System.Exit

-- For now, we shall only handle the case of empty argument list.
main = parseArgs []

-- Given our collection of arguments, we return some kind of IO action.
-- We do not specify what the IO action is exactly, because it is done
-- on a case-by-case basis.
parseArgs :: [String] -> IO a
-- In this case, the application will simply print how to run the application
-- correctly, then proceed to terminate without error. The >> operation is
-- similar to >>= except that >> ignores the input. In this case, since
-- exitSuccess is independent of printUsage, we ignore printUsage's result (which
-- is () because printUsage prints to standard IO and quits).
parseArgs [] = printUsage >> exitSuccess
-- TODO: Handle case of non-empty argument list

printUsage = putStrLn "No files given. Please run as ./asciilines file.tvg"
