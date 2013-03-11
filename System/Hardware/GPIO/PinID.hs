-- | Definition of different pin ID types.

module System.Hardware.GPIO.PinID (
        HWID(..)
      , UID(..)
) where



-- | Newtype wrapper for 'Int' for GPIO pin hardware IDs. The hardware ID is
--   what the OS uses to talk to the GPIO pins, for example the @12@ in
--   @gpio\/gpio12\/value@.
--
--   The Show instance prefixes the numbers with a '#'.
newtype HWID = HWID Int
      deriving (Eq, Ord)

instance Show HWID where
      show (HWID i) = '#' : show i



-- | The user ID is used by the architecture module to provide a more
--   user-friendly ID.
--
--   The Show instance prints it in square braces.
newtype UID a = UID a
      deriving (Eq, Ord)

instance (Show u) => Show (UID u) where
      show (UID u) = "[" ++ show u ++ "]"
