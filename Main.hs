module Main where

import System.Hardware.HPIO
import System.Hardware.HPIO.Architecture.Board

import Control.Concurrent
import Control.Monad.Trans

main = runGPIO Board $ do
      addPin (HWID 4) (UID 0) Out
      setPinValue (UID 0) Hi
      liftIO $ threadDelay $ 10^6
      setPinValue (UID 0) Lo




-- #############################################################################
-- ## MORSE PROGRAM ############################################################
-- #############################################################################

--import qualified Morse

--main :: IO ()
--main = Morse.main

-- #############################################################################
-- ## END MORSE PROGRAM ########################################################
-- #############################################################################
