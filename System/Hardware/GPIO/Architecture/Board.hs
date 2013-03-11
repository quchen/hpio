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

addPin :: (Show u, Ord u)
       => PinsRef u
       -> HWID
       -> UID u
       -> PinDirection
       -> IO ()
addPin pinsRef hwid uid dir = do
      pins <- readIORef pinsRef
      if Map.member uid pins
            then error $ printf "Pin %s already exists" (show uid)
            else do newPin <- High.construct hwid dir
                    writeIORef pinsRef $ Map.insert uid newPin pins

removePin :: (Show u, Ord u)
          => PinsRef u
          -> UID u
          -> IO ()
removePin pinsRef uid = do
      pins <- readIORef pinsRef
      if Map.member uid pins
            then do maybe (return ()) High.destruct $ Map.lookup uid pins
                    -- TODO: If one of the two actions here fails, what
                    --       should the other one do?
                    writeIORef pinsRef $ Map.delete uid pins
            else error $ printf "Pin %s does not exist" (show uid)

setPinValue :: (Show u, Ord u)
            => PinsRef u
            -> UID u
            -> PinValue
            -> IO ()
setPinValue pinsRef uid value = do
      pins <- readIORef pinsRef
      case Map.lookup uid pins of
            Just hiPin -> High.setValue hiPin value
            Nothing -> error $ printf "Pin %s does not exist" (show uid)

getPinValue :: (Show u, Ord u)
            => PinsRef u
            -> UID u
            -> IO PinValue
getPinValue pinsRef uid = do
      pins <- readIORef pinsRef
      case Map.lookup uid pins of
            Just hiPin -> High.getValue hiPin
            Nothing -> error $ printf "Pin %s does not exist" (show uid)

setPinDirection :: (Show u, Ord u)
                => PinsRef u
                -> UID u
                -> PinDirection
                -> IO ()
setPinDirection pinsRef uid dir = do
      pins <- readIORef pinsRef
      case Map.lookup uid pins of
            Just hiPin -> High.setDirection hiPin dir
            Nothing -> error $ printf "Pin %s does not exist" (show uid)

getPinDirection :: (Show u, Ord u)
                => PinsRef u
                -> UID u
                -> IO PinDirection
getPinDirection pinsRef uid = do
      pins <- readIORef pinsRef
      case Map.lookup uid pins of
            Just hiPin -> High.getDirection hiPin
            Nothing -> error $ printf "Pin %s does not exist" (show uid)
