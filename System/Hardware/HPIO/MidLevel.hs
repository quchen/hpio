-- | The mid level part of the library unifies properties of a pin in a single
--   data type.
--
--   The following program initializes pin #4 and turns it on for a second:
--
--   > import Control.Concurrent (threadDelay)
--   > import Control.Exception (bracket)
--   >
--   > blink :: HWID -> IO ()
--   > blink hwid = bracket (construct hwid Out) nuke $ \pin ->
--   >       do setValue pin Hi
--   >          threadDelay $ 10^6
--   >          setValue pin Lo
--   >
--   > main = blink $ HWID 4

-- TODO: Remove calls to error and add proper exception handling.
--       => IO errors, Either, Maybe?

module System.Hardware.HPIO.MidLevel (

      -- * Construction, destruction, existence
        construct
      , destruct
      , exists
      , absorb
      , nuke


      -- * Getters/setters
      , setValue
      , getValue
      , setDirection
      , getDirection

      -- * Data type
      , Pin(..)

) where



import qualified System.Hardware.HPIO.LowLevel as Low
import System.Hardware.HPIO.PinID
import System.Hardware.HPIO.BasicPin

import Control.Monad



-- | 'Pin' (for /high level pin/) unifies the hardware ID and a handle to the
--   value field of that pin.
--
data Pin = Pin { hiPinHWID   :: HWID
               , hiPinValueH :: ValueHandle
               }


-- | Creates a pin with the specified 'HWID' and 'Direction'.
--
--   The initial value is set to 'Lo' when the direction is set to 'Out'.
construct :: HWID -> PinDirection -> IO Pin
construct hwid dir = do assertExists False hwid
                        h <- Low.export hwid
                        Low.setDirection hwid dir
                        when (dir == Out) $ Low.setValue h Lo
                        return $ Pin hwid h


-- | Deallocates a pin. Requires the pin to exist.
destruct :: Pin -> IO ()
destruct (Pin hwid h) = assertExists True hwid >> Low.unexport hwid h



-- | Deallocates a pin whether or not it exists. This is useful in conjunction
--   with @bracket@ to deallocate a pin at the end no matter what.
--
--   > bracket (construct (HWID 0) Out)
--   >         nuke
--   >         doStuff
nuke :: Pin -> IO ()
nuke (Pin hwid h) = Low.nuke hwid h



-- | Checks whether the pin with a certain 'HWID' is currently exported.
exists :: HWID -> IO Bool
exists = Low.exists



-- | Asserts that a certain pin should/should not exist.
assertExists :: Bool -> HWID -> IO ()
assertExists should hwid = do
      ex <- exists hwid
      case (ex, should) of
            (True, False) -> error $ "Pin " ++ show hwid ++ " already exists"
            (False, True) -> error $ "Pin " ++ show hwid ++ " does not exist"
            _otherwise    -> return ()



-- | Creates a pin from one that is already open in the file system.
--
--   Note that using this can lead to two programs manipulating the same pin,
--   which is potentially troublesome. Be careful using this!
absorb :: HWID -> IO Pin
absorb hwid = do assertExists True hwid
                 h <- Low.absorb hwid
                 return $ Pin hwid h



-- | Updates the value of a pin, given its direction is 'Out'.
setValue :: Pin -> PinValue -> IO ()
setValue pin@(Pin hwid h) v = do assertExists True hwid
                                 dir <- getDirection pin
                                 let err = "Attempt to set value of *input*"
                                         ++" pin " ++ show hwid
                                 case dir of Out -> Low.setValue h v
                                             In  -> error err



-- | Reads the value of a pin, given its direction is 'In'.
getValue :: Pin -> IO PinValue
getValue pin@(Pin hwid h) = do assertExists True hwid
                               dir <- getDirection pin
                               let err = "Attempt to get value of *output*"
                                      ++ " pin " ++ show hwid
                               case dir of In  -> Low.getValue h
                                           Out -> error err



-- | Updates the direction of a pin.
--
--   Note that the underlying pin's value handle is created in 'ReadWriteMode'
--   already, so there is no need to change the handle by this function.
setDirection :: Pin -> PinDirection -> IO ()
setDirection (Pin hwid _) dir = do assertExists True hwid
                                   Low.setDirection hwid dir



-- | Reads the direction of a pin.
--
--   Note that the underlying pin's value handle is created in 'ReadWriteMode'
--   already, so there is no need to change the handle by this function.
getDirection :: Pin -> IO PinDirection
getDirection (Pin hwid _) = do assertExists True hwid
                               Low.getDirection hwid


