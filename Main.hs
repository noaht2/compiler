module Main where
import Test

main :: IO ()
main = writeFile "test.mms" (test tri_exp)
-- main = helper z_tri_exp
-- main = print (test2 z_tri_exp)
