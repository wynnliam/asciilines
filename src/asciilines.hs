import System.Environment
import System.IO
import System.Exit
import Data.String

data CanvasRow = CanvasRow [Char]
data Canvas = Canvas Integer Integer [CanvasRow]

-- For now, we shall only handle the case of empty argument list.
main = getArgs >>= parseArgs

-- Given our collection of arguments, we return some kind of IO action.
-- We do not specify what the IO action is exactly, because it is done
-- on a case-by-case basis.
parseArgs :: [String] -> IO a
-- In this case, the application will simply print how to run the application
-- correctly, then proceed to terminate without error. The >> operation is
-- similar to >>= except that >> ignores the input. In this case, since
-- exitSuccess is independent of printUsage, we ignore printUsage's result (which
-- is () because printUsage prints to standard IO and quits).
parseArgs []  = printUsage >> exitSuccess
-- TODO: Finish me!
parseArgs [file] = renderCanvasFromFile (verifyFileName file) >> exitSuccess

printUsage = putStrLn "No files given. Please run as ./asciilines file.tvg"

-- Returns Just fileName if the file has a .tvg extension.
-- Otherwise will return Nothing.
verifyFileName :: String -> Maybe String
verifyFileName file
    -- Name has to be at least ".tvg". If not then return Nothing.
    | (length file) < 4 = Nothing
    -- If the last three characters of the string are tvg, then we can read this.
    | (drop ((length file) - 3) file) == "tvg" = Just file
    -- Otherwise we return nothing.
    | otherwise = Nothing


-- TODO: Hammer this out some more
renderCanvasFromFile Nothing     = putStrLn "The file you supplied is not a tvg file. Please update the extension to be one."
renderCanvasFromFile (Just file) = (fmap show (loadTvgContents file)) >>= putStrLn

-- Returns every line of the TVG file in a list
loadTvgContents :: String -> IO [String]
loadTvgContents file = fmap lines (readFile file)

parseTvgInput :: String -> [String]
parseTvgInput rawFileInput = lines rawFileInput
