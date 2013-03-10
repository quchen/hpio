-- | The low level part of the library handles conncetions to the GPIO pins
--   on an OS level, i.e. it communicates with /sys/class/GPIO. As it basically
--   is an abstraction for "echo 1 > /sys/class/gpio/export" type commands, it
--   makes no attempt at handling bad input beyond what the Haskell type system
--   provides.

module System.Hardware.GPIO.LowLevel (

      -- * Initialization, deallocation
        export
      , unexport

      -- * Setters/getters

      , valueSet
      , valueGet

      , directionSet
      , directionGet

)where

import Text.Printf
import System.IO
import Data.Functor

import System.Hardware.GPIO.HWID
import System.Hardware.GPIO.Pin

-- | Line feed
lf :: String
lf = "\n"

-- | Path to the export handler
gpioExport :: String
gpioExport = "/sys/class/gpio/export"
-- | Path to the unexport handler
gpioUnexport :: String
gpioUnexport = "/sys/class/gpio/unexport"
-- | Path to the value handler of a pin
gpioValue :: Int -> String
gpioValue i = printf "/sys/class/gpio/gpio%d/value" i
-- | Path to the direction handler of a pin
gpioDirection :: Int -> String
gpioDirection i = printf "/sys/class/gpio/gpio%d/direction" i

-- | Exports (connects) a GPIO pin (by writing to /sys/class/gpio/export), and
--   returns a pointer to the value file generated.
--
--   Fails if the pin is already exported.
export :: HWID -> IO ValueHandle
export (HWID i) = do writeFile gpioExport $ show i ++ lf
                     ValueHandle <$> openFile (gpioValue i) ReadWriteMode

-- | Unexports (disconnects) a GPIO pin (by writing to
--   /sys/class/gpio/unexport).
--
--   Fails if the pin isn't exported.
--
--   TODO: This does not check whether there's still an open ValueHandle to the
--         value file. Is this a problem?
unexport :: HWID -> ValueHandle -> IO ()
unexport (HWID i) (ValueHandle h) = do hClose h
                                       writeFile gpioUnexport $ show i ++ lf

-- | Sets the value of a GPIO pin.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in read mode
valueSet :: ValueHandle -> PinValue -> IO ()
valueSet (ValueHandle h) value = do seekBegin h
                                    hPrint h $ show value


-- | Reads the value of a GPIO pin. Interprets anything that's not starting with
--   @1@ as 'Lo'.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in write mode
valueGet :: ValueHandle -> IO PinValue
valueGet (ValueHandle h) = do seekBegin h
                              toPinValue <$> hGetContents h

-- | Sets the direction of a GPIO pin.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in read mode
directionSet :: HWID -> PinDirection -> IO ()
directionSet (HWID i) d = writeFile (gpioDirection i) $ show d ++ lf

-- | Reads the value of a GPIO pin. Interprets anything that's not @in@ as
--   'Out'.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in write mode
directionGet :: HWID -> IO PinDirection
directionGet (HWID i) = toPinDirection <$> readFile (gpioDirection i)



-- | Makes the handle point to the beginning of the file
seekBegin :: Handle -> IO ()
seekBegin h = hSeek h AbsoluteSeek 0
