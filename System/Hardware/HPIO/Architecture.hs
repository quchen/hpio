{-# LANGUAGE TypeFamilies #-}

module System.Hardware.HPIO.Architecture (
        Architecture(..)
) where




import System.Hardware.HPIO.PinID (UID, HWID)
import System.Hardware.HPIO.BasicPin

import Data.Functor
import Control.Monad.Trans.RWS



-- | An architecture unifies all the GPIO pin functions of a certain device,
--   for example manipulating GPIO on a Raspberry Pi, or in a terminal.
class Architecture a where

      -- | Data type to store UID -> Pin associations.
      data Pins uid :: *

      -- | Construct a new architecture, containing only an empty record.
      construct :: RWST a () (Pins uid) IO ()

      -- | Deallocate all allocated pins by calling 'removePin' on them.
      destruct :: RWST a () (Pins uid) IO ()

      -- | Deallocate all pins, ignoring exceptions. Useful for bracketing, so
      --   that the pins are released no matter what when the program finishes.
      nuke :: RWST a () (Pins uid) IO ()

      -- | Adds a new pin to the system.
      addPin :: (Ord uid, Show uid) => HWID -> UID uid -> PinDirection -> RWST a () (Pins uid) IO ()

      -- | Incorporates an already open pin into the system.
      absorbPin :: (Ord uid, Show uid) => HWID -> UID uid -> RWST a () (Pins uid) IO ()

      -- | Deallocates a pin from the system.
      removePin :: (Ord uid, Show uid) => UID uid -> RWST a () (Pins uid) IO ()

      -- | Checks whether a pin is currently allocated in the 'Pins' object.
      --   Note that this does /not/ check whether the pin is present on an OS
      --   level, only whether this 'Pins' knows about it.
      isOpen :: (Ord uid, Show uid) => UID uid -> RWST a () (Pins uid) IO Bool

      -- | Checks whether a pin is constructable, i.e. is neither open on a
      --   hardware level nor known to the current 'Pins'. Success means that
      --   "construct" should not yield an error.
      isConstructable :: (Ord uid, Show uid) => UID uid -> HWID -> RWST a () (Pins uid) IO Bool

      -- | Updates the value of a pin.
      setPinValue :: (Ord uid, Show uid) => UID uid -> PinValue -> RWST a () (Pins uid) IO ()

      -- | Gets the value of a pin.
      getPinValue :: (Ord uid, Show uid) => UID uid -> RWST a () (Pins uid) IO PinValue

      -- | Sets the direction of a pin.
      setPinDirection :: (Ord uid, Show uid) => UID uid -> PinDirection -> RWST a () (Pins uid) IO ()

      -- | Gets the direction of a pin.
      getPinDirection :: (Ord uid, Show uid) => UID uid -> RWST a () (Pins uid) IO PinDirection
