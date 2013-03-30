{-# LANGUAGE TypeFamilies #-}

module System.Hardware.HPIO.Architecture.Terminal (
      Terminal(..)
) where

import System.Hardware.HPIO.PinID
import System.Hardware.HPIO.BasicPin

import System.Hardware.HPIO.Architecture (Pins, Architecture, GPIOAction)
import qualified System.Hardware.HPIO.Architecture as A

import Text.Printf
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.RWS
import Control.Monad.Trans

data SimulationPin = SimulationPin -- TODO: Make this useful

data Terminal = Terminal

instance Architecture Terminal where

      newtype Pins uid = Pins (Map (UID uid) SimulationPin)

      runGPIO         = runGPIO

      addPin          = addPin
      absorbPin       = absorbPin
      removePin       = removePin
      isOpen          = isOpen
      isConstructable = isConstructable
      setPinValue     = setPinValue
      getPinValue     = getPinValue
      setPinDirection = setPinDirection
      getPinDirection = getPinDirection



runGPIO :: a -> GPIOAction a uid () -> IO ()
runGPIO a gpio = void $ runRWST (gpio >> destruct) a (Pins Map.empty)

destruct :: GPIOAction a uid ()
destruct = liftIO $ putStrLn "Deallocating all known pins"

nuke :: GPIOAction a uid ()
nuke = liftIO $ putStrLn "Nuking all known pins"

addPin :: (Ord uid, Show uid) => HWID -> UID uid -> PinDirection -> GPIOAction a uid ()
addPin hwid uid dir = liftIO $ printf msg (show hwid) (show uid) (show dir)
      where msg = "Adding pin %s as %s; direction: %s\n"

absorbPin :: (Ord uid, Show uid) => HWID -> UID uid -> GPIOAction a uid ()
absorbPin hwid uid = liftIO $ printf msg (show hwid) (show uid)
      where msg = "Absorbing pin %s as %s\n"

removePin :: (Ord uid, Show uid) => UID uid -> GPIOAction a uid ()
removePin uid = liftIO $ printf "Removing pin %s\n" (show uid)

isOpen :: (Ord uid, Show uid) => UID uid -> GPIOAction a uid Bool
isOpen uid = liftIO $ printf msg (show uid) >> return True
      where msg = "Checking whether pin %s is open; return dummy True\n"

isConstructable :: (Ord uid, Show uid) => UID uid -> HWID -> GPIOAction a uid Bool
isConstructable uid hwid = liftIO $ printf msg (show uid) (show hwid) >> return True
      where msg = "Checking whether pin %s as %s is constructable;\n " ++
                  "return dummy True"

setPinValue :: (Ord uid, Show uid) => UID uid -> PinValue -> GPIOAction a uid ()
setPinValue uid v = liftIO $ printf msg (show uid) (show v)
      where msg = "Setting value of pin %s to %s\n"


getPinValue :: (Ord uid, Show uid) => UID uid -> GPIOAction a uid PinValue
getPinValue uid = liftIO $ printf msg (show uid) >> return Hi
      where msg = "Getting value of pin %s; return dummy Hi\n"

setPinDirection :: (Ord uid, Show uid) => UID uid -> PinDirection -> GPIOAction a uid ()
setPinDirection uid dir = liftIO $ printf msg (show uid) (show dir)
      where msg = "Setting direction of pin %s to %s\n"

getPinDirection :: (Ord uid, Show uid) => UID uid -> GPIOAction a uid PinDirection
getPinDirection uid = liftIO $ printf msg (show uid) >> return Out
      where msg = "Getting direction of pin %s; return dummy Out\n"
