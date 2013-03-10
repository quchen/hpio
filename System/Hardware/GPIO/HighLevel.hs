-- | The low high part of the library unifies properties of a pin in a single
--   data type.

-- TODO: Remove calls to error and add proper exception handling.
--       => IO errors, Either, Maybe?

module System.Hardware.GPIO.HighLevel (

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

      , HWID(..)
      , PinDirection(..)
      , PinValue(..)
      , HiPin(..)

) where

import System.Hardware.GPIO.LowLevel ( HWID
                                     , ValueHandle
                                     , PinDirection(..)
                                     , PinValue(..)
                                     )
import qualified System.Hardware.GPIO.LowLevel as Low



import Control.Monad



-- | 'HiPin' (for **Hi**gh level **Pin**) unifies 'HWID', value handle and pin
--   direction of a low-level pin.
data HiPin = HiPin { hiPinHWID :: HWID
                   , hiPinValueH :: ValueHandle
                   }

-- | Creates a pin with the specified 'HWID' and 'Direction'.
--
--   The initial value is set to 'Lo' when the direction is set to 'Out'.
construct :: HWID -> PinDirection -> IO HiPin
construct hwid dir = do assertExists False hwid
                        h <- Low.export hwid
                        Low.setDirection hwid dir
                        when (dir == Out) $ Low.setValue h Lo
                        return $ HiPin hwid h


-- | Deallocates a pin. Requires the pin to exist.
destruct :: HiPin -> IO ()
destruct pin@(HiPin hwid h) = assertExists True hwid >> Low.unexport hwid h



-- | Deallocates a pin whether or not it exists. Issues no exceptions.
nuke :: HiPin -> IO ()
nuke (HiPin hwid h) = Low.nuke hwid h



-- | Checks whether a pin is currently exported
exists :: HWID -> IO Bool
exists = Low.exists



-- | Asserts that a certain pin should/should not exist.
assertExists :: Bool -> HWID -> IO ()
assertExists should hwid = do
      e <- exists hwid
      when (e && not should) $ error $ "Pin " ++ show hwid ++ " already exists"
      when (not e && should) $ error $ "Pin " ++ show hwid ++ " does not exist"



-- | Creates a pin from one that is already open in the file system.
--
--   Note that using this can lead to two programs manipulating the same pin,
--   which could be a big cause of trouble. Only use this if you know what
--   you're doing!
absorb :: HWID -> IO HiPin
absorb hwid = do assertExists True hwid
                 h <- Low.absorb hwid
                 return $ HiPin hwid h



-- | Updates the value of a pin.
setValue :: HiPin -> PinValue -> IO ()
setValue pin@(HiPin hwid h) v = do assertExists True hwid
                                   dir <- getDirection pin
                                   let err = "Attempt to set value of *input*"
                                           ++" pin " ++ show hwid
                                   case dir of Out -> Low.setValue h v
                                               In  -> error err



-- | Reads the value of a pin.
getValue :: HiPin -> IO PinValue
getValue pin@(HiPin hwid h) = do assertExists True hwid
                                 dir <- getDirection pin
                                 let err = "Attempt to get value of *output*"
                                        ++ " pin " ++ show hwid
                                 case dir of In  -> Low.getValue h
                                             Out -> error err



-- | Updates the direction of a pin.
setDirection :: HiPin -> PinDirection -> IO ()
setDirection (HiPin hwid _) dir = do assertExists True hwid
                                     Low.setDirection hwid dir



-- | Reads the direction of a pin.
getDirection :: HiPin -> IO PinDirection
getDirection (HiPin hwid _) = do assertExists True hwid
                                 Low.getDirection hwid


