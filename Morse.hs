module Morse (
      main
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.List
import System.IO
import Text.Printf
import Data.Char (toLower)
import Control.Concurrent
import Control.Exception

import System.Hardware.GPIO



timePrefactor, timeDit, timeDah, atomSpacer, letterSpacer, wordSpacer :: Int
timePrefactor = 10 ^ 5
timeDit      = timePrefactor *  3
timeDah      = timePrefactor *  9
atomSpacer   = timePrefactor *  2
letterSpacer = timePrefactor * 10
wordSpacer   = timePrefactor * 20



main :: IO ()
main = bracket makePin nuke morseLoop
      where makePin = construct (HWID 4) Out



morseLoop :: HiPin -> IO ()
morseLoop pin = do putStr "Enter message: "
                   hFlush stdout
                   msg <- getLine
                   unless (null msg) $ do morse pin msg
                                          morseLoop pin



type MorseCommand = ReaderT HiPin IO ()



-- | Synonym for 'threadDelay'.
pause :: Int -> IO ()
pause = threadDelay



-- | Morses a string to a GPIO pin represented by a 'HiPin'.
morse :: HiPin -> String -> IO ()
morse pin message = runReaderT (morseString message) pin



-- | Takes a list of things, and a morse action acting on its elements. It
--   intersperses pauses, and then sequences the whole thing to become one large
--   morsing action.
--
--   Read this as a typechecking version of
--   @sequence_ . intersperse spacer . map action@.
combineMorses :: (a -> MorseCommand) -> [a] -> Int -> MorseCommand
combineMorses action list spacer = sequence_ $ intersperse pauses morses
      where morses = map action list
            pauses = lift $ pause spacer



-- | Morse command (blinks and pauses) of a string.
morseString :: String -> MorseCommand
morseString message = combineMorses morseWord messageWords wordSpacer
      where allowedChars = ['a'..'z']
            messageWords = map clean $ words message
            clean = filter (`elem` allowedChars) . map toLower



-- | Morse command (blinks and pauses) of a single word.
morseWord :: String -> MorseCommand
morseWord message = combineMorses morseLetter message letterSpacer



-- | Morse command (blinks and pauses) of a char.
morseLetter :: Char -> MorseCommand
morseLetter char = printLetter >> combineMorses morseAtom morseChar atomSpacer
      where printLetter = lift $ printf "%c = %s\n" char (showML morseChar)
            morseChar = toMorse char



-- | Morse command for a single atom (dit/dah).
morseAtom :: MorseAtom -> MorseCommand
morseAtom atom = do pin <- ask
                    lift $ blink pin atom

blink :: HiPin -> MorseAtom -> IO ()
blink pin atom = do setValue pin Hi
                    pause $ case atom of Dit -> timeDit
                                         Dah -> timeDah
                    setValue pin Lo



data MorseAtom = Dit | Dah

instance Show MorseAtom where
      show Dit = "."
      show Dah = "-"



type MorseLetter = [MorseAtom]



-- | Converts a 'MorseLetter' to a readable string.
--
--   > showML $ toMorse 'a' = ".-"
--   > showML $ toMorse 'b' = "-..."
--   > showML $ toMorse 'c' = "-.-."
showML :: MorseLetter -> String
showML c = c >>= show



toMorse :: Char -> MorseLetter
toMorse 'a' = [Dit, Dah]
toMorse 'b' = [Dah, Dit, Dit, Dit]
toMorse 'c' = [Dah, Dit, Dah, Dit]
toMorse 'd' = [Dah, Dit, Dit]
toMorse 'e' = [Dit]
toMorse 'f' = [Dit, Dit, Dah, Dit]
toMorse 'g' = [Dah, Dah, Dit]
toMorse 'h' = [Dit, Dit, Dit, Dit]
toMorse 'i' = [Dit, Dit]
toMorse 'j' = [Dit, Dah, Dah, Dah]
toMorse 'k' = [Dit, Dah, Dit]
toMorse 'l' = [Dit, Dah, Dit, Dit]
toMorse 'm' = [Dah, Dah]
toMorse 'n' = [Dah, Dit]
toMorse 'o' = [Dah, Dah, Dah]
toMorse 'p' = [Dit, Dah, Dah, Dit]
toMorse 'q' = [Dah, Dah, Dit, Dah]
toMorse 'r' = [Dit, Dah, Dit]
toMorse 's' = [Dit, Dit, Dit]
toMorse 't' = [Dah]
toMorse 'u' = [Dit, Dit, Dah]
toMorse 'v' = [Dit, Dit, Dit, Dah]
toMorse 'w' = [Dit, Dah, Dah]
toMorse 'x' = [Dah, Dit, Dit, Dah]
toMorse 'y' = [Dah, Dit, Dah, Dah]
toMorse 'z' = [Dah, Dah, Dit, Dit]
toMorse _   = []
