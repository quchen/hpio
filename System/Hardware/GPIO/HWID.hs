-- | Definition of the HWID (hardware GPIO pin ID) data type.

module System.Hardware.GPIO.HWID where

-- | Newtype wrapper for 'Int' for GPIO pin hardware IDs. The hardware ID is
--   what the OS uses to talk to the GPIO pins, for example the @12@ in
--   @gpio/gpio12/value@.
newtype HWID = HWID Int
      deriving (Eq, Ord)

instance Show HWID where
      show (HWID i) = '#' : show i
