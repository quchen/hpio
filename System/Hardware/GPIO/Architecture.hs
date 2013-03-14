-- | Unify multiple pins
--
-- TODO: Absorb a high level pin into a PinBox

module System.Hardware.GPIO.Architecture
where



import System.Hardware.GPIO.HighLevel ( PinValue
                                      , PinDirection
                                      )
import System.Hardware.GPIO.PinID
import System.Hardware.GPIO.PinsRef



import qualified System.Hardware.GPIO.Architecture.Board as Board
import qualified System.Hardware.GPIO.Architecture.Terminal as Terminal



import Data.Map as Map
import Data.IORef
import Data.Functor



-- | The Architecture type is used to provide a common interface for multiple
--   pins taken together. Apart from unification, it translates a custom ID to
--   the underlying hardware ID.
data Architecture = Board
                  | Terminal

-- | A 'PinBox' object stores the architecture alongside the pins allocated on
--   it.
data PinBox uid = PinBox Architecture (PinsRef uid)



-- | Initializes a new 'PinBox' using the specified architecture.
construct :: Architecture -> IO (PinBox uid)
construct arch = PinBox arch <$> newIORef Map.empty


-- | Destruct the board, deallocating all open pins.
destruct :: PinBox uid -> IO ()
destruct (PinBox Board    pinsRef) = Board.destruct pinsRef
destruct (PinBox Terminal pinsRef) = Terminal.destruct pinsRef



-- | Nuke the board, deallocating all open pins, disregarding exceptions.
--   Useful for bracketing to ressources are released no matter what.
nuke :: PinBox uid -> IO ()
nuke (PinBox Board    pinsRef) = Board.nuke pinsRef
nuke (PinBox Terminal pinsRef) = Terminal.nuke pinsRef




addPin :: (Show uid, Ord uid)
       => PinBox uid
       -> HWID
       -> UID uid
       -> PinDirection
       -> IO ()
addPin (PinBox Board    pinsRef) = Board.addPin pinsRef
addPin (PinBox Terminal pinsRef) = Terminal.addPin pinsRef


addHiPin :: (Show uid, Ord uid)
         => PinBox uid
         -> HiPin
         -> UID uid
         -> IO ()
addPin (PinBox Board    pinsRef) = Board.addHiPin pinsRef
addPin (PinBox Terminal pinsRef) = Terminal.addHiPin pinsRef



removePin :: (Show uid, Ord uid)
          => PinBox uid
          -> UID uid
          -> IO ()
removePin (PinBox Board    pinsRef) = Board.removePin pinsRef
removePin (PinBox Terminal pinsRef) = Terminal.removePin pinsRef



setPinValue :: (Show uid, Ord uid)
            => PinBox uid
            -> UID uid
            -> PinValue
            -> IO ()
setPinValue (PinBox Board    pinsRef) = Board.setPinValue pinsRef
setPinValue (PinBox Terminal pinsRef) = Terminal.setPinValue pinsRef



getPinValue :: (Show uid, Ord uid)
            => PinBox uid
            -> UID uid
            -> IO PinValue
getPinValue (PinBox Board    pinsRef) = Board.getPinValue pinsRef
getPinValue (PinBox Terminal pinsRef) = Terminal.getPinValue pinsRef



setPinDirection :: (Show uid, Ord uid)
                => PinBox uid
                -> UID uid
                -> PinDirection
                -> IO ()
setPinDirection (PinBox Board    pinsRef) = Board.setPinDirection pinsRef
setPinDirection (PinBox Terminal pinsRef) = Terminal.setPinDirection pinsRef



getPinDirection :: (Show uid, Ord uid)
                => PinBox uid
                -> UID uid
                -> IO PinDirection
getPinDirection (PinBox Board    pinsRef) = Board.getPinDirection pinsRef
getPinDirection (PinBox Terminal pinsRef) = Terminal.getPinDirection pinsRef
