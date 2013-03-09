module Main where

import Morse

import System.Hardware.GPIO


main = do let hwid = HWID 4
          putStrLn "Export"
          export hwid
          putStrLn "Setting out"
          directionSet hwid Out

          let msg = "hello world"
          putStrLn $ "Morsing \"" ++ msg ++ "\""
          morse hwid msg


          putStrLn "Unexport"
          unexport hwid
