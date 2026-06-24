module Main where 

import Criterion.Main 
import Prelude as P 
import qualified ListFusion as F 


prelude  :: Int -> Int 
prelude n = P.sum (P.map (\ x -> x * x) [1..n])

own :: Int -> Int 
own n = F.sum (F.map (\x -> x * x) (F.enumFromTo 1 n))


main :: IO ()
main =
    defaultMain 
      [  bench "prelude" (nf prelude 1000000000)
      ,  bench "own"     (nf own     1000000000)
      ]