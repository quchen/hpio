module Main where

import System.Hardware.HPIO
import System.Hardware.HPIO.Architecture.Board

import Control.Concurrent
import Control.Monad.Trans

foo = do
      addPin (HWID 4) (UID 0) Out
      setPinValue (UID 0) Hi
      liftIO $ threadDelay $ 10^6
      setPinValue (UID 0) Lo

main = runGPIO board foo




-- #############################################################################
-- ## MORSE PROGRAM ############################################################
-- #############################################################################

--import qualified Morse

--main :: IO ()
--main = Morse.main

-- #############################################################################
-- ## END MORSE PROGRAM ########################################################
-- #############################################################################
