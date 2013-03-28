{-# LANGUAGE TypeFamilies #-}

module System.Hardware.HPIO.Architecture.Terminal (
      Terminal(..)
) where

import System.Hardware.HPIO.PinID
import System.Hardware.HPIO.BasicPin

import System.Hardware.HPIO.Architecture (PinsR(..), Architecture)
import qualified System.Hardware.HPIO.Architecture as A

import Text.Printf
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef


data SimulationPin = SimulationPin -- TODO: Make this useful

newtype Terminal uid = Terminal uid

instance (Ord uid, Show uid) => Architecture (Terminal uid) where

      newtype PinsR uid = PinsR (IORef (Map (UID uid) SimulationPin))

      --              v-- "terminal" dummy parameter
      construct       _ = construct
      destruct        _ = destruct
      nuke            _ = nuke
      addPin          _ = addPin
      absorbPin       _ = absorbPin
      removePin       _ = removePin
      isOpen          _ = isOpen
      isConstructable _ = isConstructable
      setPinValue     _ = setPinValue
      getPinValue     _ = getPinValue
      setPinDirection _ = setPinDirection
      getPinDirection _ = getPinDirection



construct :: IO (PinsR uid)
construct = PinsR <$> newIORef Map.empty

destruct :: PinsR uid -> IO ()
destruct _pinsR = putStrLn "Deallocating all known pins"

nuke :: PinsR uid -> IO ()
nuke _pinsR = putStrLn "Nuking all known pins"

addPin :: (Ord uid, Show uid) => PinsR uid -> HWID -> UID uid -> PinDirection -> IO ()
addPin _pinsR hwid uid dir = printf msg (show hwid) (show uid) (show dir)
      where msg = "Adding pin %s as %s; direction: %s"

absorbPin :: (Ord uid, Show uid) => PinsR uid -> HWID -> UID uid -> PinDirection -> IO ()
absorbPin _pinsR hwid uid dir = printf msg (show hwid) (show uid) (show dir)
      where msg = "Adding pin %s as %s; direction: %s"

removePin :: (Ord uid, Show uid) => PinsR uid -> UID uid -> IO ()
removePin _pinsR uid = printf "Removing pin %s" (show uid)

isOpen :: (Ord uid, Show uid) => PinsR uid -> UID uid -> IO Bool
isOpen _pinsR uid = printf msg (show uid) >> return True
      where msg = "Checking whether pin %s is open; return dummy True"

isConstructable :: (Ord uid, Show uid) => PinsR uid -> UID uid -> HWID -> IO Bool
isConstructable _pinsR uid hwid = printf msg (show uid) (show hwid) >> return True
      where msg = "Checking whether pin %s as %s is constructable; " ++
                  "return dummy True"

setPinValue :: (Ord uid, Show uid) => PinsR uid -> UID uid -> PinValue -> IO ()
setPinValue _pinsR uid v = printf msg (show uid) (show v)
      where msg = "Setting value of pin %s to %s"


getPinValue :: (Ord uid, Show uid) => PinsR uid -> UID uid -> IO PinValue
getPinValue _pinsR uid = printf msg (show uid) >> return Hi
      where msg = "Getting value of pin %s; return dummy Hi"

setPinDirection :: (Ord uid, Show uid) => PinsR uid -> UID uid -> PinDirection -> IO ()
setPinDirection _pinsR uid dir = printf msg (show uid) (show dir)
      where msg = "Setting direction of pin %s to %s"

getPinDirection :: (Ord uid, Show uid) => PinsR uid -> UID uid -> IO PinDirection
getPinDirection _pinsR uid = printf msg (show uid) >> return Out
      where msg = "Getting direction of pin %s; return dummy Out"
