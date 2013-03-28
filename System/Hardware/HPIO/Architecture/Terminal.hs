module System.Hardware.HPIO.Architecture.Terminal (
      Terminal(..)
) where

import System.Hardware.HPIO.PinID
import System.Hardware.HPIO.BasicPin

import System.Hardware.HPIO.Architecture (PinsR(..), Architecture)
import qualified System.Hardware.HPIO.Architecture as A

import Text.Printf




newtype Terminal uid = Terminal uid

instance (Ord uid, Show uid) => Architecture (Terminal uid) where
      --              v-- "terminal" dummy parameter
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





destruct :: PinsR uid -> IO ()
destruct _ = putStrLn "Deallocating all known pins"

nuke :: PinsR uid -> IO ()
nuke _ = putStrLn "Nuking all known pins"

addPin :: (Ord uid, Show uid) => PinsR uid -> HWID -> UID uid -> PinDirection -> IO ()
addPin _ hwid uid dir = printf msg (show hwid) (show uid) (show dir)
      where msg = "Adding pin %s as %s; direction: %s"

absorbPin :: (Ord uid, Show uid) => PinsR uid -> HWID -> UID uid -> PinDirection -> IO ()
absorbPin _ hwid uid dir = printf msg (show hwid) (show uid) (show dir)
      where msg = "Adding pin %s as %s; direction: %s"

removePin :: (Ord uid, Show uid) => PinsR uid -> UID uid -> IO ()
removePin _ uid = printf "Removing pin %s" (show uid)

isOpen :: (Ord uid, Show uid) => PinsR uid -> UID uid -> IO Bool
isOpen _ uid = printf msg (show uid) >> return True
      where msg = "Checking whether pin %s is open; return dummy True"

isConstructable :: (Ord uid, Show uid) => PinsR uid -> UID uid -> HWID -> IO Bool
isConstructable _ uid hwid = printf msg (show uid) (show hwid) >> return True
      where msg = "Checking whether pin %s as %s is constructable; " ++
                  "return dummy True"

setPinValue :: (Ord uid, Show uid) => PinsR uid -> UID uid -> PinValue -> IO ()
setPinValue _ uid v = printf msg (show uid) (show v)
      where msg = "Setting value of pin %s to %s"


getPinValue :: (Ord uid, Show uid) => PinsR uid -> UID uid -> IO PinValue
getPinValue _ uid = printf msg (show uid) >> return Hi
      where msg = "Getting value of pin %s; return dummy Hi"

setPinDirection :: (Ord uid, Show uid) => PinsR uid -> UID uid -> PinDirection -> IO ()
setPinDirection _ uid dir = printf msg (show uid) (show dir)
      where msg = "Setting direction of pin %s to %s"

getPinDirection :: (Ord uid, Show uid) => PinsR uid -> UID uid -> IO PinDirection
getPinDirection _ uid = printf msg (show uid) >> return Out
      where msg = "Getting direction of pin %s; return dummy Out"
