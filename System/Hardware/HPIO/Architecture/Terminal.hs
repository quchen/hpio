-- | TODO

module System.Hardware.HPIO.Architecture.Terminal
where

import System.Hardware.HPIO.HighLevel
import System.Hardware.HPIO.PinID


destruct :: PinsRef uid -> IO ()
destruct = undefined

nuke :: PinsRef uid -> IO ()
nuke = undefined

addPin :: (Show u, Ord u)
       => PinsRef u
       -> HWID
       -> UID u
       -> PinDirection
       -> IO ()
addPin = undefined


addHiPin :: (Show uid, Ord uid)
         => HiPin
         -> UID uid
         -> IO ()
addHiPin = undefined -- TODO



removePin :: (Show u, Ord u)
          => PinsRef u
          -> UID u
          -> IO ()
removePin = undefined



setPinValue :: (Show u, Ord u)
            => PinsRef u
            -> UID u
            -> PinValue
            -> IO ()
setPinValue = undefined



getPinValue :: (Show u, Ord u)
            => PinsRef u
            -> UID u
            -> IO PinValue
getPinValue = undefined



setPinDirection :: (Show u, Ord u)
                => PinsRef u
                -> UID u
                -> PinDirection
                -> IO ()
setPinDirection = undefined



getPinDirection :: (Show u, Ord u)
                => PinsRef u
                -> UID u
                -> IO PinDirection
getPinDirection = undefined
