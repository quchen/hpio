module System.Hardware.HPIO.Architecture.Board
where

import qualified System.Hardware.HPIO.MidLevel as Mid
import System.Hardware.HPIO.BasicPin
import System.Hardware.HPIO.PinID


import System.Hardware.HPIO.Architecture (Architecture, Pins(Pins))
import qualified System.Hardware.HPIO.Architecture as A

import Text.Printf
import Data.IORef
import qualified Data.Map as Map
import Data.Functor
import Control.Monad
import Data.Traversable



board :: Architecture uid
board = A.defaultArchitecture {
        A.destruct        = destruct
      , A.nuke            = nuke
      , A.addPin          = addPin
      , A.absorbPin       = undefined
      , A.removePin       = undefined
      , A.isOpen          = undefined
      , A.isConstructable = undefined
      , A.setPinValue     = undefined
      , A.getPinValue     = undefined
      , A.setPinDirection = undefined
      , A.getPinDirection = undefined
}

-- | Sequences an IO action over all pins contained in a Pins object. Used as
--   a common interface for "destruct" and "nuke".
sequenceAllPins :: (Mid.Pin -> IO ()) -> Pins uid -> IO ()
sequenceAllPins f (Pins p) = void $ readIORef p >>= traverse f

destruct :: Pins uid -> IO ()
destruct = sequenceAllPins Mid.destruct

nuke :: Pins uid -> IO ()
nuke = sequenceAllPins Mid.nuke

addPin :: (Ord uid) => Pins uid-> HWID-> UID uid-> PinDirection-> IO ()
addPin (Pins pinsRef) hwid uid dir = do
      -- Check existence
      ex <- Mid.exists hwid
      when ex $ error "Pin already allocated on the hardware"
      -- Check previous allocation
      p <- readIORef pinsRef
      when (uid `Map.member` p) $ error "Pin already in the pins list"
      -- Everything fine, allocate
      newPin <- Mid.construct hwid dir
      writeIORef pinsRef $ Map.insert uid newPin p

--construct :: HWID -> PinDirection -> IO Pin



--newtype Pins uid = Pins (IORef (Map (UID uid) Pin))




--nuke :: Pins uid -> IO ()
--nuke pins = void $ Map.map Mid.nuke <$> readIORef pins
--
--
--
--addPin :: (Show uid, Ord uid)
--       => Pins uid
--       -> HWID
--       -> UID uid
--       -> PinDirection
--       -> IO ()
--addPin pins hwid uid dir = do
--      pins <- readIORef pins
--      if Map.member uid pins
--            then error $ printf "Pin %s already exists" (show uid)
--            else do newPin <- Mid.construct hwid dir
--                    writeIORef pins $ Map.insert uid newPin pins

--addHiPin :: (Show uid, Ord uid)
--         => HiPin
--         -> UID uid
--         -> IO ()
--addHiPin = undefined -- TODO

--removePin :: (Show uid, Ord uid)
--          => Pins uid
--          -> UID uid
--          -> IO ()
--removePin pins uid = do
--      pins <- readIORef pins
--      if Map.member uid pins
--            then do maybe (return ()) Mid.destruct $ Map.lookup uid pins
--                    -- TODO: If one of the two actions here fails, what
--                    --       should the other one do?
--                    writeIORef pins $ Map.delete uid pins
--            else error $ printf "Pin %s does not exist" (show uid)

--setPinValue :: (Show uid, Ord uid)
--            => Pins uid
--            -> UID uid
--            -> PinValue
--            -> IO ()
--setPinValue pins uid value = do
--      pins <- readIORef pins
--      case Map.lookup uid pins of
--            Just hiPin -> Mid.setValue hiPin value
--            Nothing -> error $ printf "Pin %s does not exist" (show uid)

--getPinValue :: (Show uid, Ord uid)
--            => Pins uid
--            -> UID uid
--            -> IO PinValue
--getPinValue pins uid = do
--      pins <- readIORef pins
--      case Map.lookup uid pins of
--            Just hiPin -> Mid.getValue hiPin
--            Nothing -> error $ printf "Pin %s does not exist" (show uid)

--setPinDirection :: (Show uid, Ord uid)
--                => Pins uid
--                -> UID uid
--                -> PinDirection
--                -> IO ()
--setPinDirection pins uid dir = do
--      pins <- readIORef pins
--      case Map.lookup uid pins of
--            Just hiPin -> Mid.setDirection hiPin dir
--            Nothing -> error $ printf "Pin %s does not exist" (show uid)

--getPinDirection :: (Show uid, Ord uid)
--                => Pins uid
--                -> UID uid
--                -> IO PinDirection
--getPinDirection pins uid = do
--      pins <- readIORef pins
--      case Map.lookup uid pins of
--            Just hiPin -> Mid.getDirection hiPin
--            Nothing -> error $ printf "Pin %s does not exist" (show uid)
