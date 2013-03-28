module System.Hardware.HPIO.Architecture (
      PinsR(..),
      Architecture(..)
) where


import Data.Functor
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map


import System.Hardware.HPIO.PinID (UID, HWID)
import System.Hardware.HPIO.BasicPin
import System.Hardware.HPIO.MidLevel (Pin)


-- | PinsR is a wrapper for an 'IORef' to be used as a global state of open pins
--   of a device.
--
--   The "Show" instance required for many functions is to provide better error
--   messages.
newtype PinsR uid = PinsR (IORef (Map (UID uid) Pin))


-- | An architecture unifies all the GPIO pin functions of a certain device,
--   for example manipulating GPIO on a Raspberry Pi, or in a terminal.
class Architecture a where

      -- | Construct a new architecture, containing only an empty record.
      construct :: a -> IO (PinsR uid)
      construct a = PinsR <$> newIORef Map.empty

      -- | Deallocate all allocated pins by calling 'removePin' on them.
      destruct :: a -> PinsR uid -> IO ()

      -- | Deallocate all pins, ignoring exceptions. Useful for bracketing, so
      --   that the pins are released no matter what when the program finishes.
      nuke :: a -> PinsR uid -> IO ()

      -- | Adds a new pin to the system.
      addPin :: (Ord uid, Show uid) => a -> PinsR uid -> HWID -> UID uid -> PinDirection -> IO ()

      -- | Incorporates an already open pin into the system.
      absorbPin :: (Ord uid, Show uid) => a -> PinsR uid -> HWID -> UID uid -> PinDirection -> IO ()

      -- | Deallocates a pin from the system.
      removePin :: (Ord uid, Show uid) => a -> PinsR uid -> UID uid -> IO ()

      -- | Checks whether a pin is currently allocated in the 'PinsR' object.
      --   Note that this does /not/ check whether the pin is present on an OS
      --   level, only whether this 'PinsR' knows about it.
      isOpen :: (Ord uid, Show uid) => a -> PinsR uid -> UID uid -> IO Bool

      -- | Checks whether a pin is constructable, i.e. is neither open on a
      --   hardware level nor known to the current 'PinsR'. Success means that
      --   "construct" should not yield an error.
      isConstructable :: (Ord uid, Show uid) => a -> PinsR uid -> UID uid -> HWID -> IO Bool

      -- | Updates the value of a pin.
      setPinValue :: (Ord uid, Show uid) => a -> PinsR uid -> UID uid -> PinValue -> IO ()

      -- | Gets the value of a pin.
      getPinValue :: (Ord uid, Show uid) => a -> PinsR uid -> UID uid -> IO PinValue

      -- | Sets the direction of a pin.
      setPinDirection :: (Ord uid, Show uid) => a -> PinsR uid -> UID uid -> PinDirection -> IO ()

      -- | Gets the direction of a pin.
      getPinDirection :: (Ord uid, Show uid) => a -> PinsR uid -> UID uid -> IO PinDirection
