-- | The low level part of the library handles conncetions to the GPIO pins
--   on an OS level, i.e. it communicates with /sys/class/GPIO. As it basically
--   is an abstraction for "echo 1 > /sys/class/gpio/export" type commands, it
--   makes no attempt at handling bad input beyond what the Haskell type system
--   provides.

module System.Hardware.GPIO.LowLevel (

      -- * Initialization, deallocation
        export
      , unexport
      , exists
      , absorb
      , nuke

      -- * Setters/getters

      , setValue
      , getValue

      , setDirection
      , getDirection

      , module System.Hardware.GPIO.HWID
      , module System.Hardware.GPIO.Pin

) where



import Text.Printf
import System.IO
import Data.Functor
import System.Directory
import System.FilePath
import Control.Exception (catch, SomeException)
import Prelude hiding (catch)

import System.Hardware.GPIO.HWID
import System.Hardware.GPIO.Pin


gpio :: String
gpio = "/sys/class/gpio"
-- | Path to the export handler
gpioExport :: String
gpioExport = gpio </> "export"
-- | Path to the unexport handler
gpioUnexport :: String
gpioUnexport = gpio </> "unexport"
-- | Path to the GPIO directory of a pin
gpioDirectory :: Int -> String
gpioDirectory i = gpio </> gpio ++ show i
-- | Path to the value handler of a pin
gpioValue :: Int -> String
gpioValue i = gpioDirectory i </> "value"
-- | Path to the direction handler of a pin
gpioDirection :: Int -> String
gpioDirection i = gpioDirectory i </> "direction"



-- | Exports (connects) a GPIO pin (by writing to /sys/class/gpio/export), and
--   returns a pointer to the value file generated.
--
--   Fails if the pin is already exported.
export :: HWID -> IO ValueHandle
export hwid@(HWID i) = do writeFile gpioExport $ show i ++ "\n"
                          absorb hwid



-- | Creates a handle to the value of an already existing pin.
--
--   Be careful not to absorb a pin created by some other program, or the
--   interference may be nasty!
absorb :: HWID -> IO ValueHandle
absorb (HWID i) = ValueHandle <$> openFile (gpioValue i) ReadWriteMode



-- | Checks whether a GPIO pin currently exists, i.e. was exported. Note that
--   this is done on an OS level, and there is no test whether the pin may have
--   been allocated by some other program.
exists :: HWID -> IO Bool
exists (HWID i) = doesFileExist $ gpioDirectory i



-- | Unexports (disconnects) a GPIO pin (by writing to
--   /sys/class/gpio/unexport).
--
--   Fails if the pin isn't exported.
--
--   TODO: This does not check whether there's still an open ValueHandle to the
--         value file. Is this a problem?
unexport :: HWID -> ValueHandle -> IO ()
unexport (HWID i) (ValueHandle h) = do hClose h
                                       writeFile gpioUnexport $ show i ++ "\n"


-- | Deallocates a pin whether or not it exists; does not issue any exceptions.
--   This can be useful as part of bracketing, in order to deallocate the pin
--   no matter what.
nuke :: HWID -> ValueHandle -> IO ()
nuke (HWID i) (ValueHandle h) = do ignoreException $ hClose h
                                   ignoreException $ writeFile gpioUnexport $
                                                     show i ++ "\n"
      where ignoreException f = catch f doNothing
            doNothing :: SomeException -> IO ()
            doNothing _ = return ()
-- TODO: The catchall above may not be the ideal solution; instead, possible
--       exceptions should be ignored individually. See the docs for further
--       information:
--       http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Exception.html#g:4


-- | Sets the value of a GPIO pin.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in read mode
setValue :: ValueHandle -> PinValue -> IO ()
setValue (ValueHandle h) value = seekBegin h >> hPrint h value >> hFlush h



-- | Reads the value of a GPIO pin. Interprets anything that's not starting with
--   @1@ as 'Lo'.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in write mode
getValue :: ValueHandle -> IO PinValue
getValue (ValueHandle h) = seekBegin h >> toPinValue <$> hGetContents h



-- | Sets the direction of a GPIO pin.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in read mode
setDirection :: HWID -> PinDirection -> IO ()
setDirection (HWID i) d = writeFile (gpioDirection i) $ show d ++ "\n"



-- | Reads the value of a GPIO pin. Interprets anything that's not @in@ as
--   'Out'.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in write mode
getDirection :: HWID -> IO PinDirection
getDirection (HWID i) = toPinDirection <$> readFile (gpioDirection i)



-- | Makes the handle point to the beginning of the file
seekBegin :: Handle -> IO ()
seekBegin h = hSeek h AbsoluteSeek 0
