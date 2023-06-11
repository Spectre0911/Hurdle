{-# LANGUAGE LambdaCase, TypeApplications #-}

module CLI where

import Hurdle
import Control.Monad ( forever, zipWithM_, replicateM )
import System.Random (randomRIO)
import Data.Char (chr, ord, toUpper, toLower)
import System.Console.ANSI (SGR(SetColor, Reset), ConsoleLayer (Foreground, Background), ColorIntensity (Vivid, Dull), Color (Green, Yellow, White, Black, Red, Magenta), setSGR, setSGRCode, hClearScreen, clearScreenCode)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode (NoBuffering))
import System.Environment (getArgs)
import Hurdle.Command
import Data.List
import Data.Function
import Data.Ord (comparing)
import Data.Maybe
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings, InputT, outputStrLn, getInputChar, outputStr)
import Control.Monad.IO.Class (liftIO)
import Hurdle.Match
import Hurdle.Words

import Text.Read (readMaybe)

-- Run the command line program.
runCLI :: IO ()
runCLI = do
    t <- getArgs
    case t of
        [a] -> case readMaybe @Int a of
            Nothing -> runInputT defaultSettings loop
            Just n  -> generateTestCases n >>= mapM_ putStrLn

        _   -> runInputT defaultSettings loop

-- Print a particular output code.
sgr :: [SGR] -> InputT IO ()
sgr = outputStr . setSGRCode

-- Run the game loop.
loop :: InputT IO ()
loop = do
    rn <- liftIO $ randomRIO (0, length answerList - 1)
    let chosenWord = answerList !! rn

        y s = setSGRCode [SetColor Foreground Vivid Yellow ] ++ s
        m s = setSGRCode [SetColor Foreground Vivid Magenta] ++ s

    outputStr clearScreenCode

    outputStrLn $ unlines ["",
        y "8                        8" ++ m " o.     " ++ y "       ",
        y "8                        8" ++ m " `8.    " ++ y "       ",
        y "8oPYo. o    o oPYo. .oPYo8" ++ m "  `8.   " ++ y ".oPYo. ",
        y "8    8 8    8 8  `' 8    8" ++ m "  .88.  " ++ y "8oooo8 ",
        y "8    8 8    8 8     8    8" ++ m " .8 `8. " ++ y "8.     ",
        y "8    8 `YooP' 8     `YooP'" ++ m " 8'  `8." ++ y "`Yooo' "
        ]

    sgr [Reset]

    outputStrLn "------------------"
    play (normalise chosenWord) []

    outputStrLn "------------------"
    sgr [Reset]
    outputStrLn "Press any key to play again"
    getInputChar ""

    loop

-- Play a single round of the game.
play :: String -> [String] -> InputT IO ()
play answer history = do
    input <- parseCommand . fromMaybe "" <$> getInputLine "Guess a word: "

    case input of
      GiveUp -> do
          outputStrLn $ "Gave up after " ++ show (length history) ++ " guesses."
          outputStr "The actual word was "
          sgr [SetColor Foreground Vivid Magenta]
          outputStr answer
          sgr [Reset]
          outputStrLn "."
      ShowLetters -> do
          let
            colorHistory = map (maximumBy (comparing snd))
                . groupBy ((==) `on` fst) . sort
                . concatMap (uncurry zip)
            ch = colorHistory $ map (\w -> (w,matchingAlgo w answer)) history
            lcol = \case
                Nothing   -> setSGRCode [Reset]
                Just None -> setSGRCode [SetColor Background Vivid Black,       
                                         SetColor Foreground Dull Black]
                Just m    -> matchColorCode m
            colorize c = lcol (lookup c ch) ++ pure c ++ setSGRCode [Reset]
          outputStrLn $ unwords $ map colorize "QWERTYUIOP"
          outputStr " "
          outputStrLn $ unwords $ map colorize "ASDFGHJKL"
          outputStr "  "
          outputStrLn $ unwords $ map colorize "ZXCVBNM"
          play answer history
      Guess g' -> do
        let g = normalise g'
        if isValid g
            then do
                let m = matchingAlgo g answer
                printGuess m g
                outputStrLn ""
                if m == replicate 5 Exact
                    then outputStrLn $
                        "Well done! Got it in "
                        ++ show (length history + 1)
                        ++ " guesses."
                    else play answer (g:history)
            else do
                sgr [SetColor Foreground Vivid Red]
                outputStrLn "Sorry, that's not a valid guess. Try again."
                sgr [Reset]
                play answer history

printGuess :: [Match] -> String -> InputT IO ()
printGuess m c = zipWithM_ f m c
    where
        box c = chr $ ord '\x1f130' + ord c - ord 'a'
        f m c = do
            outputStr $ matchColorCode m
            outputStr $ box (toLower c) : " "
            sgr [Reset]

-- Get the foreground/background colour code corresponding to a match.
matchColorCode :: Match -> String
matchColorCode m = setSGRCode $ case m of
    Exact   -> [SetColor Background Dull Green , SetColor Foreground Dull Black]
    Partial -> [SetColor Background Dull Yellow, SetColor Foreground Dull Black]
    None    -> [Reset]


-- Generate some example test cases.
generateTestCases :: Int -> IO [String]
generateTestCases n = replicateM (min 100 n) $ do
    rn <- liftIO $ randomRIO (0, length answerList)
    let chosenWord = normalise $ answerList !! rn

    rg <- liftIO $ randomRIO (0, length answerList)
    let guess = normalise $ answerList !! rg

    let m = getMatches (exactMatches chosenWord guess) guess
    pure $ show (chosenWord, guess, m)
