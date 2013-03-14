module System.Hardware.GPIO.PinsRef (
      PinsRef
) where


-- | Unification of multiple pins. Will also translate between user and hardware
--   IDs.
type PinsRef a = IORef (Map (UID a) HiPin)
