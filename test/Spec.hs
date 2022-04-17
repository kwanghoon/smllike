module Main where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import Config

import Data.Maybe (isJust)
import System.IO (readFile)

spec = hspec $ do
  describe "Arith expression" $ do
    
    let config_simple = True
    
    let config =
          Configuration
            {
              config_SIMPLE       = config_simple,
              config_R_LEVEL      = 1,
              config_GS_LEVEL     = 5,
              config_DEBUG        = False,
              config_DISPLAY      = False,
              config_PRESENTATION = 0,
              config_ALGORITHM    = 3
            }
    
    let benchmark1 = "./app/example/ex1.sml"
    
    it ("[Benchmark1] ex1.sml") $
      do item benchmark1 config 

    let benchmark2 = "./app/example/ex2.sml"
    
    it ("[Benchmark2] ex2.sml") $
      do item benchmark2 config 

    let benchmark3 = "./app/example/test1.sml"
    
    it ("[Benchmark3] test1.sml") $
      do item benchmark3 config 

    let benchmark4 = "./app/example/test2.sml"
    
    it ("[Benchmark4] test2.sml") $
      do item benchmark4 config 

    let benchmark5 = "./app/example/test3.sml"
    
    it ("[Benchmark5] test3.sml") $
      do item benchmark5 config 

    let benchmark6 = "./app/example/test4.sml"
    
    it ("[Benchmark6] test4.sml") $
      do item benchmark6 config 

item benchmark_file init_config  = 
      do let test_config = init_config
         putStrLn (show test_config)
         
         configMaybe <- readConfig
         benchmark <- readFile benchmark_file
         case configMaybe of
           Just config ->
             do writeConfig test_config  -- set
                results <- computeCand False benchmark "" (config_SIMPLE test_config)
                writeConfig init_config       -- restore
                
           Nothing -> isJust configMaybe `shouldBe` True

  
main :: IO ()
main = spec
  
