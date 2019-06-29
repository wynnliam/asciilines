import System.Environment
import System.IO
import System.Exit
import Data.String
import Text.Read

data CanvasDim = CanvasDim Int Int deriving (Show)

data Orientation = Horizontal | Vertical deriving (Show, Read)
data Line = Line Orientation Integer deriving (Show, Read)
data Command = Command Char Integer Integer Line deriving (Show, Read)
data TvgData = TvgData CanvasDim [Command] deriving (Show)

data Canvas = Canvas CanvasDim [[Char]]

instance Show Canvas where
    show (Canvas _ canvas) = unlines canvas

createCanvas :: CanvasDim -> Canvas
createCanvas (CanvasDim rowSize colSize) = Canvas (CanvasDim rowSize colSize) canvas
    where canvas = replicate rowSize (replicate colSize '.')

drawSymbolOnRow :: Char -> Int -> [Char] -> [Char]
drawSymbolOnRow symbol col row
    | col < 0 = row
    | col >= (length row) = row
    | otherwise = fstHalf ++ sndHalf
        where fstHalf = fst splitRow
              sndHalf = [symbol] ++ (tail . snd) splitRow
              splitRow = splitAt col row

drawVerticalLine :: Char -> Int -> [[Char]] -> [[Char]]
drawVerticalLine symbol col canvas = map (drawSymbolOnRow symbol col) canvas

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

renderCanvasFromFile Nothing         = putStrLn "The file you supplied is not a tvg file. Please update the extension to be one."
--renderCanvasFromFile (Just file)     = (getTvgCommands file) >>= tempPrint
renderCanvasFromFile (Just file)     = (getTvgCommands file) >>= tempPrint

getTvgCommands file = fmap parseTvgContents (fmap lines (readFile file))

parseTvgContents :: [String] -> TvgData
parseTvgContents (x : xs) = TvgData (buildCanvasDims x) (parseTvgCommands xs)

buildCanvasDims :: String -> CanvasDim
buildCanvasDims line = CanvasDim (head nums) ((head . tail) nums)
    where nums = map (read::String->Int) (words line)

parseTvgCommands :: [String] -> [Command]
parseTvgCommands commandStrs = map ((read::String->Command).convertCommandToReadable) commandStrs

-- Converts a line that looks like "symbol num num h/v num" to
-- "Command 'symbol' num num (Line Horizontal/Vertical num)"
-- so we can use the read function
convertCommandToReadable :: String -> String
convertCommandToReadable line = "Command " ++ symbol ++ " " ++ row ++ " " ++ col ++ " " ++ "(Line " ++ lineOrientation ++ " " ++ lineLength ++ ")"
    where symbol = "'" ++ (tokens!!0) ++ "'"
          row = tokens!!1
          col = tokens!!2
          lineOrientation = orientation (tokens!!3)
          lineLength = tokens!!4
          tokens = words line

orientation "h" = "Horizontal"
orientation "v" = "Vertical"

tempPrint :: TvgData -> IO ()
tempPrint tvgData = putStrLn (show tvgData)
