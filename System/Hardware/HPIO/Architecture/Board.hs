{-# LANGUAGE TypeFamilies #-}

module System.Hardware.HPIO.Architecture.Board (
      Board(..)
) where

import qualified System.Hardware.HPIO.MidLevel as Mid
import System.Hardware.HPIO.BasicPin
import System.Hardware.HPIO.PinID



import qualified System.Hardware.HPIO.Architecture as A

import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Functor
import Control.Monad
import Data.Traversable
import Data.Maybe
import Control.Applicative


newtype Board uid = Board uid



instance (Ord uid) => A.Architecture (Board uid) where

      newtype PinsR uid = PinsR (IORef (Map (UID uid) Mid.Pin))

      --              v-- "board" dummy parameter
      A.construct       _ = construct
      A.destruct        _ = destruct
      A.nuke            _ = nuke
      A.addPin          _ = addPin
      A.absorbPin       _ = absorbPin
      A.removePin       _ = removePin
      A.isOpen          _ = isOpen
      A.isConstructable _ = isConstructable
      A.setPinValue     _ = setPinValue
      A.getPinValue     _ = getPinValue
      A.setPinDirection _ = setPinDirection
      A.getPinDirection _ = getPinDirection


construct :: IO (PinsR uid)
construct = PinsR <$> newIORef Map.empty


-- | Performs an IO action on all pins contained in a PinsR object. Used as a
--   common interface for "destruct" and "nuke".
traversePins :: (Mid.Pin -> IO ()) -> PinsR uid -> IO ()
traversePins f (PinsR pins) = void $ readIORef pins >>= traverse f

destruct :: PinsR uid -> IO ()
destruct = traversePins Mid.destruct

nuke :: PinsR uid -> IO ()
nuke = traversePins Mid.nuke

addPin :: (Ord uid) => PinsR uid -> HWID -> UID uid -> PinDirection -> IO ()
addPin (PinsR pinsR) hwid uid dir = do
      -- Check existence
      ex <- Mid.exists hwid
      when ex $ error "Pin already allocated on the hardware"
      -- Check previous allocation
      p <- readIORef pinsR
      when (uid `Map.member` p) $ error "Pin already in the pins list"
      -- Everything fine, allocate
      newPin <- Mid.construct hwid dir
      writeIORef pinsR $ Map.insert uid newPin p

absorbPin :: (Ord uid) => PinsR uid -> HWID -> UID uid -> PinDirection -> IO ()
absorbPin (PinsR pinsR) hwid uid dir = do
      -- Check whether pin is already allocated
      ex <- Mid.exists hwid
      when (not ex) $ error "Pin not allocated on the hardware, cannot absorb"
      -- Check previous addition to the known pins
      p <- readIORef pinsR
      when (uid `Map.member` p) $ error "Pin already in the pins list"
      -- Everything fine, absorb
      newPin <- Mid.absorb hwid
      writeIORef pinsR $ Map.insert uid newPin p

removePin :: (Ord uid) => PinsR uid -> UID uid -> IO ()
removePin (PinsR pinsR) uid = do
      p <- readIORef pinsR
      case Map.lookup uid p of
            Just pin -> do Mid.destruct pin
                           writeIORef pinsR (Map.delete uid p)
            Nothing  -> error "Pin not in pins list"

isOpen :: (Ord uid) => PinsR uid -> UID uid -> IO Bool
isOpen (PinsR pinsR) uid = readIORef pinsR >>= return . isJust . Map.lookup uid

isConstructable :: (Ord uid) => PinsR uid -> UID uid -> HWID -> IO Bool
isConstructable pins uid hwid = liftA2 nor open ex
      where nor x y = not (x && y)
            open    = isOpen pins uid
            ex      = Mid.exists hwid

setPinValue :: (Ord uid) => PinsR uid -> UID uid -> PinValue -> IO ()
setPinValue (PinsR pinsR) uid v = do
      p <- readIORef pinsR
      case Map.lookup uid p of
            Just pin -> Mid.setValue pin v
            Nothing  -> error "Pin not in pins list"

getPinValue :: (Ord uid) => PinsR uid -> UID uid -> IO PinValue
getPinValue (PinsR pinsR) uid = do
      p <- readIORef pinsR
      case Map.lookup uid p of
            Just pin -> Mid.getValue pin
            Nothing  -> error "Pin not in pins list"

setPinDirection :: (Ord uid) => PinsR uid -> UID uid -> PinDirection -> IO ()
setPinDirection (PinsR pinsR) uid dir = do
      p <- readIORef pinsR
      case Map.lookup uid p of
            Just pin -> Mid.setDirection pin dir
            Nothing  -> error "Pin not in pins list"

getPinDirection :: (Ord uid) => PinsR uid -> UID uid -> IO PinDirection
getPinDirection (PinsR pinsR) uid = do
      p <- readIORef pinsR
      case Map.lookup uid p of
            Just pin -> Mid.getDirection pin
            Nothing  -> error "Pin not in pins list"
