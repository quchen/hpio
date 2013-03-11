-- | Unify multiple pins

module System.Hardware.GPIO.Architecture
where

import System.Hardware.GPIO.HighLevel ( PinValue
                                      , PinDirection
                                      , PinsRef
                                      )
import System.Hardware.GPIO.PinID

import qualified System.Hardware.GPIO.Architecture.Board as Board
import qualified System.Hardware.GPIO.Architecture.Terminal as Terminal

import Data.Map as Map
import Data.IORef

-- | The Architecture type is used to provide a common interface for multiple
--   pins taken together. Apart from unification, it translates a custom ID to
--   the underlying hardware ID.
data Architecture = Board
                  | Terminal


create :: IO (PinsRef a)
create = newIORef Map.empty

addPin :: (Show u, Ord u)
       => Architecture
       -> PinsRef u
       -> HWID
       -> UID u
       -> PinDirection
       -> IO ()
addPin Board = Board.addPin
addPin Terminal = Terminal.addPin

removePin :: (Show u, Ord u)
          => Architecture
          -> PinsRef u
          -> UID u
          -> IO ()
removePin Board = Board.removePin
removePin Terminal = Terminal.removePin


setPinValue :: (Show u, Ord u)
            => Architecture
            -> PinsRef u
            -> UID u
            -> PinValue
            -> IO ()
setPinValue Board = Board.setPinValue
setPinValue Terminal = Terminal.setPinValue

getPinValue :: (Show u, Ord u)
            => Architecture
            -> PinsRef u
            -> UID u
            -> IO PinValue
getPinValue Board = Board.getPinValue
getPinValue Terminal = Terminal.getPinValue

setPinDirection :: (Show u, Ord u)
                => Architecture
                -> PinsRef u
                -> UID u
                -> PinDirection
                -> IO ()
setPinDirection Board = Board.setPinDirection
setPinDirection Terminal = Terminal.setPinDirection

getPinDirection :: (Show u, Ord u)
                => Architecture
                -> PinsRef u
                -> UID u
                -> IO PinDirection
getPinDirection Board = Board.getPinDirection
getPinDirection Terminal = Terminal.getPinDirection
