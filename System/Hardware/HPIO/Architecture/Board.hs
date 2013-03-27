module System.Hardware.HPIO.Architecture.Board (
      Board
) where

import qualified System.Hardware.HPIO.MidLevel as Mid
import System.Hardware.HPIO.BasicPin
import System.Hardware.HPIO.PinID


import System.Hardware.HPIO.Architecture (Pins(Pins), Architecture)
import qualified System.Hardware.HPIO.Architecture as A

import Text.Printf
import Data.IORef
import qualified Data.Map as Map
import Data.Functor
import Control.Monad
import Data.Traversable
import Data.Maybe
import Control.Applicative


newtype Board uid = Board uid


instance (Ord uid) => Architecture (Board uid) where
      destruct        _board = destruct
      nuke            _board = nuke
      addPin          _board = addPin
      absorbPin       _board = absorbPin
      removePin       _board = removePin
      isOpen          _board = isOpen
      isConstructable _board = isConstructable
      setPinValue     _board = setPinValue
      getPinValue     _board = getPinValue
      setPinDirection _board = setPinDirection
      getPinDirection _board = getPinDirection

-- | Performs an IO action on all pins contained in a Pins object. Used as a
--   common interface for "destruct" and "nuke".
sequenceAllPins :: (Mid.Pin -> IO ()) -> Pins uid -> IO ()
sequenceAllPins f (Pins p) = void $ readIORef p >>= traverse f

destruct :: Pins uid -> IO ()
destruct = sequenceAllPins Mid.destruct

nuke :: Pins uid -> IO ()
nuke = sequenceAllPins Mid.nuke

addPin :: (Ord uid) => Pins uid -> HWID -> UID uid -> PinDirection -> IO ()
addPin (Pins pRef) hwid uid dir = do
      -- Check existence
      ex <- Mid.exists hwid
      when ex $ error "Pin already allocated on the hardware"
      -- Check previous allocation
      p <- readIORef pRef
      when (uid `Map.member` p) $ error "Pin already in the pins list"
      -- Everything fine, allocate
      newPin <- Mid.construct hwid dir
      writeIORef pRef $ Map.insert uid newPin p

absorbPin :: (Ord uid) => Pins uid -> HWID -> UID uid -> PinDirection -> IO ()
absorbPin (Pins pRef) hwid uid dir = do
      -- Check whether pin is already allocated
      ex <- Mid.exists hwid
      when (not ex) $ error "Pin not allocated on the hardware, cannot absorb"
      -- Check previous addition to the known pins
      p <- readIORef pRef
      when (uid `Map.member` p) $ error "Pin already in the pins list"
      -- Everything fine, absorb
      newPin <- Mid.absorb hwid
      writeIORef pRef $ Map.insert uid newPin p

removePin :: (Ord uid) => Pins uid -> UID uid -> IO ()
removePin (Pins pRef) uid = do
      p <- readIORef pRef
      case Map.lookup uid p of
            Just pin -> do Mid.destruct pin
                           writeIORef pRef (Map.delete uid p)
            Nothing  -> error "Pin not in pins list"

isOpen :: (Ord uid) => Pins uid -> UID uid -> IO Bool
isOpen (Pins pRef) uid = readIORef pRef >>= return . isJust . Map.lookup uid

isConstructable :: (Ord uid) => Pins uid -> UID uid -> HWID -> IO Bool
isConstructable pins uid hwid = liftA2 nor open ex
      where nor x y = not (x && y)
            open    = isOpen pins uid
            ex      = Mid.exists hwid

setPinValue :: (Ord uid) => Pins uid -> UID uid -> PinValue -> IO ()
setPinValue (Pins pRef) uid v = do
      p <- readIORef pRef
      case Map.lookup uid p of
            Just pin -> Mid.setValue pin v
            Nothing  -> error "Pin not in pins list"

getPinValue :: (Ord uid) => Pins uid -> UID uid -> IO PinValue
getPinValue (Pins pRef) uid = do
      p <- readIORef pRef
      case Map.lookup uid p of
            Just pin -> Mid.getValue pin
            Nothing  -> error "Pin not in pins list"

setPinDirection :: (Ord uid) => Pins uid -> UID uid -> PinDirection -> IO ()
setPinDirection (Pins pRef) uid dir = do
      p <- readIORef pRef
      case Map.lookup uid p of
            Just pin -> Mid.setDirection pin dir
            Nothing  -> error "Pin not in pins list"

getPinDirection :: (Ord uid) => Pins uid -> UID uid -> IO PinDirection
getPinDirection (Pins pRef) uid = do
      p <- readIORef pRef
      case Map.lookup uid p of
            Just pin -> Mid.getDirection pin
            Nothing  -> error "Pin not in pins list"
