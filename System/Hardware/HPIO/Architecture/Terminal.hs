{-# LANGUAGE TypeFamilies #-}

module System.Hardware.HPIO.Architecture.Terminal (
      Terminal(..)
) where

import System.Hardware.HPIO.PinID
import System.Hardware.HPIO.BasicPin

import System.Hardware.HPIO.Architecture (Pins)
import qualified System.Hardware.HPIO.Architecture as A

import Text.Printf
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.RWS
import Control.Monad.Trans

data SimulationPin = SimulationPin -- TODO: Make this useful

newtype Terminal uid = Terminal uid

instance (Ord uid, Show uid) => A.Architecture (Terminal uid) where

      newtype Pins uid = Pins (Map (UID uid) SimulationPin)

      construct       = construct
      destruct        = destruct
      nuke            = nuke
      addPin          = addPin
      absorbPin       = absorbPin
      removePin       = removePin
      isOpen          = isOpen
      isConstructable = isConstructable
      setPinValue     = setPinValue
      getPinValue     = getPinValue
      setPinDirection = setPinDirection
      getPinDirection = getPinDirection



construct :: RWST a () (Pins uid) IO ()
construct = put $ Pins Map.empty

destruct :: RWST a () (Pins uid) IO ()
destruct = liftIO $ liftIO $ putStrLn "Deallocating all known pins"

nuke :: RWST a () (Pins uid) IO ()
nuke = liftIO $ putStrLn "Nuking all known pins"

addPin :: (Ord uid, Show uid) => HWID -> UID uid -> PinDirection -> RWST a () (Pins uid) IO ()
addPin hwid uid dir = liftIO $ printf msg (show hwid) (show uid) (show dir)
      where msg = "Adding pin %s as %s; direction: %s"

absorbPin :: (Ord uid, Show uid) => HWID -> UID uid -> RWST a () (Pins uid) IO ()
absorbPin hwid uid = liftIO $ printf msg (show hwid) (show uid)
      where msg = "Absorbing pin %s as %s"

removePin :: (Ord uid, Show uid) => UID uid -> RWST a () (Pins uid) IO ()
removePin uid = liftIO $ printf "Removing pin %s" (show uid)

isOpen :: (Ord uid, Show uid) => UID uid -> RWST a () (Pins uid) IO Bool
isOpen uid = liftIO $ printf msg (show uid) >> return True
      where msg = "Checking whether pin %s is open; return dummy True"

isConstructable :: (Ord uid, Show uid) => UID uid -> HWID -> RWST a () (Pins uid) IO Bool
isConstructable uid hwid = liftIO $ printf msg (show uid) (show hwid) >> return True
      where msg = "Checking whether pin %s as %s is constructable; " ++
                  "return dummy True"

setPinValue :: (Ord uid, Show uid) => UID uid -> PinValue -> RWST a () (Pins uid) IO ()
setPinValue uid v = liftIO $ printf msg (show uid) (show v)
      where msg = "Setting value of pin %s to %s"


getPinValue :: (Ord uid, Show uid) => UID uid -> RWST a () (Pins uid) IO PinValue
getPinValue uid = liftIO $ printf msg (show uid) >> return Hi
      where msg = "Getting value of pin %s; return dummy Hi"

setPinDirection :: (Ord uid, Show uid) => UID uid -> PinDirection -> RWST a () (Pins uid) IO ()
setPinDirection uid dir = liftIO $ printf msg (show uid) (show dir)
      where msg = "Setting direction of pin %s to %s"

getPinDirection :: (Ord uid, Show uid) => UID uid -> RWST a () (Pins uid) IO PinDirection
getPinDirection uid = liftIO $ printf msg (show uid) >> return Out
      where msg = "Getting direction of pin %s; return dummy Out"
