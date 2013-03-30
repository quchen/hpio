{-# LANGUAGE TypeFamilies #-}

module System.Hardware.HPIO.Architecture (
        Architecture(..)
      , GPIOAction
) where




import System.Hardware.HPIO.PinID (UID, HWID)
import System.Hardware.HPIO.BasicPin

import Control.Monad.Trans.RWS


-- | A GPIOAction allows multiple operations to be chained together. Internally,
--   it's RWST+IO.
type GPIOAction arch uid = RWST arch () (Pins uid) IO





-- | An architecture unifies all the GPIO pin functions of a certain device,
--   for example manipulating GPIO on a Raspberry Pi, or in a terminal.
class Architecture a where

      -- | Data type to store UID -> Pin associations.
      data Pins :: * -> *

      -- | Run a chain of GPIO commands on the specified architecture.
      runGPIO :: (Ord uid)
              => a -- ^ Architecture to use
              -> GPIOAction a uid () -- ^ GPIO action to run
              -> IO ()

      -- | Adds a new pin to the system.
      addPin :: (Ord uid, Show uid)
             => HWID
             -> UID uid
             -> PinDirection
             -> GPIOAction a uid ()

      -- | Incorporates an already open pin into the system.
      absorbPin :: (Ord uid, Show uid)
                => HWID
                -> UID uid
                -> GPIOAction a uid ()

      -- | Deallocates a pin from the system.
      removePin :: (Ord uid, Show uid)
                => UID uid
                -> GPIOAction a uid ()

      -- | Checks whether a pin is currently allocated in the 'Pins' object.
      --   Note that this does /not/ check whether the pin is present on an OS
      --   level, only whether this 'Pins' knows about it.
      isOpen :: (Ord uid, Show uid)
             => UID uid
             -> GPIOAction a uid Bool

      -- | Checks whether a pin is constructable, i.e. is neither open on a
      --   hardware level nor known to the current 'Pins'. Success means that
      --   "construct" should not yield an error.
      isConstructable :: (Ord uid, Show uid)
                      => UID uid
                      -> HWID
                      -> GPIOAction a uid Bool

      -- | Updates the value of a pin.
      setPinValue :: (Ord uid, Show uid)
                  => UID uid
                  -> PinValue
                  -> GPIOAction a uid ()

      -- | Gets the value of a pin.
      getPinValue :: (Ord uid, Show uid)
                  => UID uid
                  -> GPIOAction a uid PinValue

      -- | Sets the direction of a pin.
      setPinDirection :: (Ord uid, Show uid)
                      => UID uid
                      -> PinDirection
                      -> GPIOAction a uid ()

      -- | Gets the direction of a pin.
      getPinDirection :: (Ord uid, Show uid)
                      => UID uid
                      -> GPIOAction a uid PinDirection
