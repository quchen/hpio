{-# LANGUAGE TypeFamilies #-}

module System.Hardware.HPIO.Architecture.Board (
      Board(..)
) where

import qualified System.Hardware.HPIO.MidLevel as Mid
import System.Hardware.HPIO.BasicPin
import System.Hardware.HPIO.PinID



import System.Hardware.HPIO.Architecture (Pins, Architecture, GPIOAction)
import qualified System.Hardware.HPIO.Architecture as A

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Data.Traversable
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.RWS


data Board = Board


instance Architecture Board where

      newtype Pins uid = Pins (Map (UID uid) Mid.Pin)

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
destruct = do
      (Pins pins) <- get
      liftIO $ traverse Mid.destruct pins
      put $ Pins Map.empty



nuke :: GPIOAction a uid ()
nuke = do
      (Pins pins) <- get
      liftIO $ traverse Mid.nuke pins
      put $ Pins Map.empty



addPin :: (Ord uid, Show uid) => HWID -> UID uid -> PinDirection -> GPIOAction a uid ()
addPin hwid uid dir = do
      let msg = "Pin already allocated on the hardware"
      assertExists False msg hwid

      (Pins pins) <- get

      -- Check whether the pin is already known to the program
      when (uid `Map.member` pins) $ error "Pin already known"

      -- Create and put new pin
      newPin <- liftIO $ Mid.construct hwid dir
      put $ Pins (Map.insert uid newPin pins)



assertExists :: (MonadIO m) => Bool -> String -> HWID -> m ()
assertExists should msg hwid = do
      ex <- liftIO $ Mid.exists hwid
      when (should /= ex) $ error msg
      return ()



absorbPin :: (Ord uid, Show uid) => HWID -> UID uid -> GPIOAction a uid ()
absorbPin hwid uid = do
      let msg = "Pin not allocated on the hardware, cannot absorb"
      assertExists True msg hwid

      (Pins pins) <- get

      -- Check whether the pin is already known to the program
      when (uid `Map.member` pins) $ error "Pin already known"

      -- Absorb pin
      newPin <- liftIO $ Mid.absorb hwid
      put $ Pins (Map.insert uid newPin pins)



removePin :: (Ord uid, Show uid) => UID uid -> GPIOAction a uid ()
removePin uid = do
      (Pins pins) <- get

      case Map.lookup uid pins of
            Just pin -> do put $ Pins (Map.delete uid pins)
                           liftIO $ Mid.destruct pin
            Nothing -> error "Pin unknown, skipping unexport"



isOpen :: (Ord uid, Show uid) => UID uid -> GPIOAction a uid Bool
isOpen uid = do (Pins pins) <- get
                return . isJust . Map.lookup uid $ pins



isConstructable :: (Ord uid, Show uid) => UID uid -> HWID -> GPIOAction a uid Bool
isConstructable uid hwid = liftA2 nor open ex
      where nor x y = not (x && y)
            open    = isOpen uid
            ex      = liftIO $ Mid.exists hwid



setPinValue :: (Ord uid, Show uid) => UID uid -> PinValue -> GPIOAction a uid ()
setPinValue uid v = do
      (Pins pins) <- get
      case Map.lookup uid pins of
            Just pin -> liftIO $ Mid.setValue pin v
            Nothing  -> error "Pin not in pins list"



getPinValue :: (Ord uid, Show uid) => UID uid -> GPIOAction a uid PinValue
getPinValue uid = do
      (Pins pins) <- get
      case Map.lookup uid pins of
            Just pin -> liftIO $ Mid.getValue pin
            Nothing  -> error "Pin not in pins list"



setPinDirection :: (Ord uid, Show uid) => UID uid -> PinDirection -> GPIOAction a uid ()
setPinDirection uid dir = do
      (Pins pins) <- get
      case Map.lookup uid pins of
            Just pin -> liftIO $ Mid.setDirection pin dir
            Nothing  -> error "Pin not in pins list"



getPinDirection :: (Ord uid, Show uid) => UID uid -> GPIOAction a uid PinDirection
getPinDirection uid = do
      (Pins pins) <- get
      case Map.lookup uid pins of
            Just pin -> liftIO $ Mid.getDirection pin
            Nothing  -> error "Pin not in pins list"
