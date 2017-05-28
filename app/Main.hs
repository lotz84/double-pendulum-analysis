module Main where

import Data.List (intercalate)
import Data.VectorSpace ((^-^))
import DoublePendulum (DoublePendulum(..), DoublePendulumPosition(..), move)

norm :: DoublePendulumPosition -> Double
norm (DoublePendulumPosition x y z w) = sqrt (x^2 + y^2 + z^2 + w^2)

calcLyapunov :: Double -> Double -> Double
calcLyapunov th1 th2 =
  let dp = DoublePendulum
        { _l1  = 1.0
        , _m1  = 1.0
        , _l2  = 1.0
        , _m2  = 1.0
        , _pos = undefined
        }
      pos1 = DoublePendulumPosition
        { _th1  = th1 * pi
        , _dth1 = 0.0
        , _th2  = th2 * pi
        , _dth2 = 0.0
        }
      diff = 1e-8
      pos2 = pos1
        { _th1 = _th1 pos1 + diff
        , _th2 = _th2 pos1 + diff
        }
      dp1 = dp {_pos = pos1}
      dp2 = dp {_pos = pos2}

      iterateNum :: Num a => a
      iterateNum = 1000

      dp1' = iterate (move 0.01) dp1 !! iterateNum
      dp2' = iterate (move 0.01) dp2 !! iterateNum

      delta_0 = norm (_pos dp1  ^-^ _pos dp2 )
      delta_t = norm (_pos dp1' ^-^ _pos dp2')

   in log (delta_t / delta_0) / 10.0


main :: IO ()
main = do
  let range = [-1.0, -0.99 .. 1.0]
      ls = calcLyapunov <$> range <*> range
      result = unlines $ map (\((x, y), l) -> intercalate " " [show x, show y, show l]) $ zip ((,) <$> range <*> range) ls
  writeFile "source.dat" result

