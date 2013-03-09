module Morse (
      morse
) where

import Control.Concurrent
import System.Hardware.GPIO


timeDit      = 3 * 10^5
timeDah      = 6 * 10^5
atomSpacer   = 2 * 10^5
letterSpacer = 10^6
wordSpacer   = 2 * 10^6

morse :: HWID -> String -> IO ()
morse hwid (x:xs) = morseChar x >> morse hwid xs
      where morseChar x | x `elem` ['a'..'z'] = morseLetter hwid x
                        | x == ' '            = threadDelay wordSpacer
                        | otherwise           = return ()
morse _ [] = return ()

morseLetter :: HWID -> Char -> IO ()
morseLetter hwid char = do putStrLn $ char : " = " ++ showMorseLetter (toMorse char)
                           mapM_ (morseAtom hwid) $ toMorse char
                           threadDelay letterSpacer

morseAtom :: HWID -> MorseAtom -> IO ()
morseAtom hwid Dit = blink hwid timeDit >> threadDelay atomSpacer
morseAtom hwid dah = blink hwid timeDah >> threadDelay atomSpacer

-- Makes a pin blink for some time in milliseconds
blink :: HWID -> Int -> IO ()
blink hwid ms = do valueSet hwid Hi
                   threadDelay ms
                   valueSet hwid Lo

data MorseAtom = Dit | Dah

instance Show MorseAtom where
      show Dit = "."
      show Dah = "-"

type MorseLetter = [MorseAtom]

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
