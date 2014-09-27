--file: dice.hs

import Data.List (genericLength)
import Graphics.EasyPlot (plot, TerminalType(X11), Graph2D(Data2D))

numFaces = 100
costPerRoll = 1

{-
main :: IO Bool
main = plot X11 $ Data2D [] [] ps
  where ps = map (\x -> (fromIntegral x,value x)) [1..30]
-}

main :: IO ()
main = putStrLn $ "Values: " ++ (show $ value 10000)

value :: Int -> Double
value 0 = 0
value n = (kl * (mean ks) + (numFaces - kl) * nextVal) / numFaces
  where
    -- value for next (-1) number of rolls
    nextVal = (value $ n - 1) - costPerRoll
    -- number of roll values that should be kept
    kl = genericLength ks
    -- roll values that should be kept
    ks = filter (> nextVal) [1..numFaces]

mean :: Floating a => [a] -> a
mean ns = (sum ns) / (genericLength ns)
