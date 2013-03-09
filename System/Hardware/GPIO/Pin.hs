module System.Hardware.GPIO.Pin (
        PinValue(..)
      , PinDirection(..)

      , toPinDirection
      , toPinValue
) where

data PinValue = Lo | Hi
      deriving (Eq, Ord)

instance Show PinValue where
      show Lo = "0"
      show Hi = "1"

data PinDirection = Out | In
      deriving (Eq, Ord)

instance Show PinDirection where
      show Out = "out"
      show In  = "in"

-- | Interpret anything but "in" as In.
toPinDirection "in" = In
toPinDirection _    = Out

-- | Interpret anything not starting with '1' as Lo
toPinValue ('1':_) = Hi
toPinValue _       = Lo
