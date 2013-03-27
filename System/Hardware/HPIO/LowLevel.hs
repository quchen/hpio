-- | The low level part of the library handles conncetions to the GPIO pins
--   on an OS level, i.e. it communicates with /sys/class/GPIO. As it basically
--   is an abstraction for "echo 1 > /sys/class/gpio/export" type commands, it
--   makes no attempt at handling exceptions.

module System.Hardware.HPIO.LowLevel (

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

) where



import System.IO
import Data.Functor
import System.Directory
import Control.Monad
import System.FilePath
import Control.Exception (catch, SomeException)
import Prelude hiding (catch)

import System.Hardware.HPIO.BasicPin
import System.Hardware.HPIO.PinID


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
gpioDirectory i = gpio </> "gpio" ++ show i
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



-- | Creates a handle to the value of an already existing pin. The handle is
--   opened in @ReadWriteMode@, so there is no handle-level distinction between
--   input and output pins in the lowlevel interface.
--
--   Be careful not to absorb a pin created by some other program, or the
--   interference may be nasty!
absorb :: HWID -> IO ValueHandle
absorb (HWID i) = ValueHandle <$> openFile (gpioValue i) ReadWriteMode



-- | Checks whether a GPIO pin currently exists, i.e. was exported. Note that
--   this is done on an OS level, and there is no test whether the pin may have
--   been allocated by some other program.
exists :: HWID -> IO Bool
exists (HWID i) = doesDirectoryExist $ gpioDirectory i



-- | Unexports (disconnects) a GPIO pin (by writing to
--   /sys/class/gpio/unexport).
--
--   Fails if the pin isn't exported.
--
--   TODO: This does not check whether there's still an open ValueHandle to the
--         value file. Is this a problem?
unexport :: HWID -> ValueHandle -> IO ()
unexport (HWID i) (ValueHandle h) = do isOpen <- hIsOpen h
                                       when isOpen $ hClose h
                                       writeFile gpioUnexport $ show i ++ "\n"


-- | Deallocates a pin whether or not it exists; does not issue any exceptions.
--
--   The high level interface uses this to implement its version of nuke, which
--   is useful as part of bracketing (deallocate pin whatever happens).
nuke :: HWID -> ValueHandle -> IO ()
nuke (HWID i) (ValueHandle h)  = do
      let doNothing :: SomeException -> IO ()
          doNothing _ = return ()
          close = hClose h
          unexport' = writeFile gpioUnexport $ show i ++ "\n"
      hClose h `catch` doNothing
      unexport' `catch` doNothing
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
setValue (ValueHandle h) value = seek0 h >> hPrint h value >> hFlush h



-- | Reads the value of a GPIO pin. Interprets anything that's not starting with
--   @1@ as 'Lo'.
--
--   Fails if the pin wasn't exported first.
--
--   TODO: Check whether this fails when the pin is in write mode
getValue :: ValueHandle -> IO PinValue
getValue (ValueHandle h) = seek0 h >> toPinValue <$> hGetContents h



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



-- | Seek to the beginning of the file
seek0 :: Handle -> IO ()
seek0 h = hSeek h AbsoluteSeek 0