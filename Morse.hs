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
import System.Hardware.GPIO


timeDit      =  3 * 10^5
timeDah      =  9 * 10^5
atomSpacer   =  2 * 10^5
letterSpacer = 10 * 10^5
wordSpacer   = 20 * 10^5


main = do let hwid = HWID 4
          h <- export hwid
          directionSet hwid Out
          morseLoop h
          unexport hwid h



morseLoop h = do putStr "Enter message: "
                 hFlush stdout
                 msg <- getLine
                 unless (null msg) $ do morse h msg
                                        morseLoop h

type MorseCommand = ReaderT ValueHandle IO ()

-- | Synonym for 'threadDelay'.
pause :: Int -> IO ()
pause = threadDelay

-- | Morses a string to a GPIO pin represented by a 'ValueHandle'.
morse :: ValueHandle -> String -> IO ()
morse h message = runReaderT (morseString message) h

-- | Morse command (blinks and pauses) of a string.
morseString :: String -> MorseCommand
morseString message = morsePaused
      where -- Read this where clause like an imperative program: data flow is
            -- from top to bottom.

            allowedChars = ['a'..'z']

            messageWords :: [String]
            messageWords = map (map toLower . filter (`elem` allowedChars)) .
                           words $
                           message

            -- List of words to morse
            prepareMorse :: [MorseCommand]
            prepareMorse = map morseWord messageWords

            wordPause :: MorseCommand
            wordPause = lift $ pause wordSpacer

            morsePaused :: MorseCommand
            morsePaused = sequence_ $ intersperse wordPause prepareMorse

morseWord :: String -> MorseCommand
morseWord message = morsePaused
      where -- Read this where clause like an imperative program: data flow is
            -- from top to bottom.

            prepareMorse :: [MorseCommand]
            prepareMorse = map morseLetter message

            letterPause :: MorseCommand
            letterPause = lift $ pause letterSpacer

            morsePaused = sequence_ $ intersperse letterPause prepareMorse

-- | Morse command (blinks and pauses) of a char.
morseLetter :: Char -> MorseCommand
morseLetter char = printLetter >> morsePaused
      where -- Read this where clause like an imperative program: data flow is
            -- from top to bottom.

            printLetter = lift $ printf "%c = %s\n" char (showMorseLetter $ toMorse char)

            -- Convert char to morse string
            morseChar :: [MorseAtom]
            morseChar = toMorse char

            -- Builds a list of morse commands to be issued, but doesn't
            -- actually do anything.
            prepareMorse :: [MorseCommand]
            prepareMorse = map morseAtom morseChar

            -- | ReaderT version of the letter spacer
            atomPause :: MorseCommand
            atomPause = lift $ pause atomSpacer

            -- Intersperse pauses, and run all the actions
            morsePaused :: MorseCommand
            morsePaused = sequence_ $ intersperse atomPause prepareMorse


-- | Morse command for a single atom (dit/dah).
morseAtom :: MorseAtom -> ReaderT ValueHandle IO ()
morseAtom atom = do h <- ask
                    let time Dit = timeDit
                        time Dah = timeDah
                        blink = do valueSet h Hi
                                   pause $ time atom
                                   valueSet h Lo
                    lift blink

data MorseAtom = Dit | Dah

instance Show MorseAtom where
      show Dit = "."
      show Dah = "-"

type MorseLetter = [MorseAtom]

-- | Converts a 'MorseLetter' to a readable string.
--
--   > showMorseLetter $ toMorse 'a' = ".-"
--   > showMorseLetter $ toMorse 'b' = "-..."
--   > showMorseLetter $ toMorse 'c' = "-.-."

showMorseLetter :: MorseLetter -> String
showMorseLetter c = c >>= show

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
