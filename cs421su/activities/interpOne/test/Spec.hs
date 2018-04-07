module Spec where

import System.Timeout (timeout)
import Control.Exception (try, evaluate, SomeException)

import Main hiding (main)

main :: IO ()
main = let handleTest (test, name) = do output <- runTest test
                                        putStrLn $ output ++ ": " ++ name
       in  mapM_ handleTest allTests

runTest :: [Bool] -> IO String
runTest tests
    = let test  = case and tests of
                   True  -> "Passed"
                   False -> "Failed"
          onExn = const "Exception" :: SomeException -> String
          tryT  = try (evaluate test) >>= return . either onExn id
      in  timeout 1000000 tryT >>= return . maybe "Timeout" id

allTests :: [([Bool], String)]
allTests = [ (tests_plus, "mytake")
           , (tests_max, "max")
           ]

tests_plus :: [Bool]
tests_plus = [ eval (IntOpExp "+" (IntExp 20) (IntExp 22)) [] == IntVal 42 ]

tests_max :: [Bool]
tests_max = [ eval (IntOpExp "max" (IntExp 20) (IntExp 22)) [] == IntVal 22
            , eval (IntOpExp "max" (IntExp 42) (IntExp 22)) [] == IntVal 42
            , eval (IntOpExp "max" (VarExp "a") (IntExp 22)) [("a",IntVal 99)] == IntVal 99
            , eval (IntOpExp "max" (VarExp "a") (IntOpExp "+" (VarExp "a") (IntExp 22))) [("a",IntVal 99)] == IntVal 121
            ]
