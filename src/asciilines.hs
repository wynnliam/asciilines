import System.Environment
import System.IO
import System.Exit
import Data.String
import Text.Read

data CanvasDim = CanvasDim Integer Integer deriving (Show)

data Orientation = Horizontal | Vertical deriving (Show, Read)
data Line = Line Orientation Integer deriving (Show, Read)
data Command = Command Char Integer Integer Line deriving (Show, Read)
data TvgData = TvgData CanvasDim [Command] deriving (Show)

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
renderCanvasFromFile Nothing         = putStrLn "The file you supplied is not a tvg file. Please update the extension to be one."
--renderCanvasFromFile (Just file)     = (getTvgCommands file) >>= tempPrint
renderCanvasFromFile (Just file)     = putStrLn "File"

getTvgCommands file = fmap lines (readFile file)

parseTvgContents :: [String] -> TvgData
parseTvgContents (x : xs) = TvgData (buildCanvasDims x) []

buildCanvasDims :: String -> CanvasDim
buildCanvasDims line = CanvasDim (head nums) ((head . tail) nums)
    where nums = map (read::String->Integer) (words line)

-- Converts a line that looks like "symbol num num h/v num" to
-- "Command 'symbol' num num (Line Horizontal/Vertical num)"
-- so we can use the read function
convertCommandToReadable :: String -> String
--convertCommandToReadable line = (tokens!!0) ++ (tokens!!1) ++ (tokens!!2) ++ (orientation (tokens!!3)) ++ (tokens!!4)
convertCommandToReadable line = "Command " ++ symbol ++ " " ++ row ++ " " ++ col ++ " " ++ "(Line " ++ lineOrientation ++ " " ++ lineLength ++ ")"
    where symbol = "'" ++ (tokens!!0) ++ "'"
          row = tokens!!1
          col = tokens!!2
          lineOrientation = orientation (tokens!!3)
          lineLength = tokens!!4
          tokens = words line

orientation "h" = "Horizontal"
orientation "v" = "Vertical"

tempPrint :: [String] -> IO ()
tempPrint commands = putStrLn (show commands)
