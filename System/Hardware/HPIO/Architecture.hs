

module System.Hardware.HPIO.Architecture (
        defaultArchitecture
      , Architecture(..)
      , Pins(..)
) where


import Data.Functor
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map


import System.Hardware.HPIO.PinID (UID, HWID)
import System.Hardware.HPIO.BasicPin
import System.Hardware.HPIO.MidLevel (Pin)


-- | Pins is a wrapper for an 'IORef' to be used as a global state of open pins
--   of a device.
newtype Pins uid = Pins (IORef (Map (UID uid) Pin))


-- | An architecture unifies all the GPIO pin functions of a certain device,
--   for example manipulating GPIO on a Raspberry Pi, or in a terminal.
data Architecture uid = Architecture {

      -- | Construct a new architecture, containing only an empty record.
        construct       :: IO (Pins uid)

      -- | Deallocate all allocated pins by calling 'removePin' on them.
      , destruct        :: Pins uid -> IO ()

      -- | Deallocate all pins, ignoring exceptions. Useful for bracketing, so
      --   that the pins are released no matter what when the program finishes.
      , nuke            :: Pins uid -> IO ()

      -- | Adds a new pin to the system.
      , addPin          :: Pins uid -> HWID -> UID uid -> PinDirection -> IO ()

      -- | Incorporates an already open pin into the system.
      , absorbPin       :: Pins uid -> HWID -> UID uid -> PinDirection -> IO ()

      -- | Deallocates a pin from the system.
      , removePin       :: Pins uid -> UID uid -> IO ()

      -- | Checks whether a pin is currently allocated in the 'Pins' object.
      --   Note that this does /not/ check whether the pin is present on an OS
      --   level, only whether this 'Pins' knows about it.
      , isOpen          :: Pins uid -> UID uid -> IO Bool

      -- | Checks whether a pin is constructable, i.e. has not been allocated
      --   yet on the system level.
      , isConstructable :: Pins uid -> HWID -> IO Bool

      -- | Updates the value of a pin.
      , setPinValue     :: Pins uid -> UID uid -> PinValue -> IO ()

      -- | Gets the value of a pin.
      , getPinValue     :: Pins uid -> UID uid -> IO PinValue

      -- | Sets the direction of a pin.
      , setPinDirection :: Pins uid -> UID uid -> PinDirection -> IO ()

      -- | Gets the direction of a pin.
      , getPinDirection :: Pins uid -> UID uid -> IO PinDirection

}

defaultConstruct :: IO (Pins uid)
defaultConstruct = Pins <$> newIORef Map.empty



defaultArchitecture :: Architecture uid
defaultArchitecture = Architecture {
        construct       = defaultConstruct
      , destruct        = undefined
      , nuke            = undefined
      , addPin          = undefined
      , absorbPin       = undefined
      , removePin       = undefined
      , isOpen          = undefined
      , isConstructable = undefined
      , setPinValue     = undefined
      , getPinValue     = undefined
      , setPinDirection = undefined
      , getPinDirection = undefined
}