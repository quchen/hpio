module System.Hardware.GPIO.Architecture.Board
where

import qualified System.Hardware.GPIO.HighLevel as High
import System.Hardware.GPIO.HighLevel (PinDirection
                                      , PinValue
                                      , PinsRef
                                      )
import System.Hardware.GPIO.PinID

import Text.Printf
import Data.IORef
import qualified Data.Map as Map
import Data.Functor
import Control.Monad


destruct :: PinsRef uid -> IO ()
destruct pinsRef = void $ Map.map High.destruct <$> readIORef pinsRef



nuke :: PinsRef uid -> IO ()
nuke pinsRef = void $ Map.map High.nuke <$> readIORef pinsRef



addPin :: (Show uid, Ord uid)
       => PinsRef uid
       -> HWID
       -> UID uid
       -> PinDirection
       -> IO ()
addPin pinsRef hwid uid dir = do
      pins <- readIORef pinsRef
      if Map.member uid pins
            then error $ printf "Pin %s already exists" (show uid)
            else do newPin <- High.construct hwid dir
                    writeIORef pinsRef $ Map.insert uid newPin pins

addHiPin :: (Show uid, Ord uid)
         => HiPin
         -> UID uid
         -> IO ()
addHiPin = undefined -- TODO

removePin :: (Show uid, Ord uid)
          => PinsRef uid
          -> UID uid
          -> IO ()
removePin pinsRef uid = do
      pins <- readIORef pinsRef
      if Map.member uid pins
            then do maybe (return ()) High.destruct $ Map.lookup uid pins
                    -- TODO: If one of the two actions here fails, what
                    --       should the other one do?
                    writeIORef pinsRef $ Map.delete uid pins
            else error $ printf "Pin %s does not exist" (show uid)

setPinValue :: (Show uid, Ord uid)
            => PinsRef uid
            -> UID uid
            -> PinValue
            -> IO ()
setPinValue pinsRef uid value = do
      pins <- readIORef pinsRef
      case Map.lookup uid pins of
            Just hiPin -> High.setValue hiPin value
            Nothing -> error $ printf "Pin %s does not exist" (show uid)

getPinValue :: (Show uid, Ord uid)
            => PinsRef uid
            -> UID uid
            -> IO PinValue
getPinValue pinsRef uid = do
      pins <- readIORef pinsRef
      case Map.lookup uid pins of
            Just hiPin -> High.getValue hiPin
            Nothing -> error $ printf "Pin %s does not exist" (show uid)

setPinDirection :: (Show uid, Ord uid)
                => PinsRef uid
                -> UID uid
                -> PinDirection
                -> IO ()
setPinDirection pinsRef uid dir = do
      pins <- readIORef pinsRef
      case Map.lookup uid pins of
            Just hiPin -> High.setDirection hiPin dir
            Nothing -> error $ printf "Pin %s does not exist" (show uid)

getPinDirection :: (Show uid, Ord uid)
                => PinsRef uid
                -> UID uid
                -> IO PinDirection
getPinDirection pinsRef uid = do
      pins <- readIORef pinsRef
      case Map.lookup uid pins of
            Just hiPin -> High.getDirection hiPin
            Nothing -> error $ printf "Pin %s does not exist" (show uid)
