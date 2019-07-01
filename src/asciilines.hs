import System.Environment
import System.IO
import System.Exit
import Data.String
import Text.Read

data CanvasDim = CanvasDim Int Int deriving (Show)

data Orientation = Horizontal | Vertical deriving (Show, Read)
data Line = Line Orientation Int deriving (Show, Read)
data Command = Command Char Int Int Line deriving (Show, Read)
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

drawHorizontalLine :: Char -> String -> String
drawHorizontalLine symbol canvas = map (\c -> symbol) canvas

cutCanvasRow :: Int -> Int -> String -> (String, (String, String))
cutCanvasRow start len canvasRow = (top, (cutArea, bottom))
    where top = fst splitRow
          cutArea = fst splitLowerHalf
          bottom = snd splitLowerHalf
          splitLowerHalf = splitAt len (snd splitRow)
          splitRow = splitAt start canvasRow

cutCanvas :: Int -> Int -> [String] -> ([String], ([String], [String]))
cutCanvas start len canvas = (top, (cutCanvas, post))
    where post = snd bottomSplit
          cutCanvas = fst bottomSplit
          bottomSplit =  splitAt len (snd splitCanvas)
          top = fst splitCanvas
          splitCanvas = splitAt start canvas

-- drawVerticalLine will draw a whole line across a given canvas.
-- This function is responsible for finding the "sub"canvas that
-- a vertical line command is responsible for and then using drawVerticalLine
-- to draw it.
drawVerticalCommand :: Char -> Int -> Int -> Int -> [[Char]] -> [[Char]]
drawVerticalCommand symbol row col len canvas 
        -- Without this case, we get the incorrect result. This will adjust
        -- The line to render in cases of being out of bounds.
        | row < 0 = drawVerticalCommand symbol (row + 1) col (len - 1) canvas
        | otherwise = pre ++ commandResult ++ post
    where pre = fst cut
          commandResult = drawVerticalLine symbol col ((fst . snd) cut)
          post = (snd . snd) cut
          cut = cutCanvas row len canvas

drawHorizontalCommand :: Char -> Int -> Int -> Int -> [[Char]] -> [[Char]]
drawHorizontalCommand symbol row col len canvas 
    | (row < 0) || (row >= (length canvas)) = canvas
    | col < 0 = drawHorizontalCommand symbol row (col + 1) (len - 1) canvas
    | otherwise = pre ++ commandResult ++ post
        where pre = fst canvasButCut
              canvasRowButCut = cutCanvasRow col len ((head . fst . snd) canvasButCut)
              rowDrawResult = drawHorizontalLine symbol ((fst . snd) canvasRowButCut)
              commandResult = [(fst canvasRowButCut) ++ rowDrawResult ++ ((snd . snd) canvasRowButCut)]
              post = (snd . snd) canvasButCut
              canvasButCut = cutCanvas row 1 canvas

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

executeCommands :: Canvas -> [Command] -> Canvas
executeCommands canvas [] = canvas
executeCommands canvas (c : cs) = executeCommands (executeCommand c canvas) cs

executeCommand :: Command -> Canvas -> Canvas
executeCommand (Command sym row col (Line Horizontal len)) (Canvas dimensions renderable) = (Canvas dimensions (drawHorizontalCommand sym row col len renderable))
executeCommand (Command sym row col (Line Vertical len)) (Canvas dimensions renderable) = (Canvas dimensions (drawVerticalCommand sym row col len renderable))

tempPrint :: TvgData -> IO ()
tempPrint tvgData = putStrLn (show tvgData)
