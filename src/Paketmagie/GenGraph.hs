module Paketmagie.GenGraph where

import System.Random
import Paketmagie.Graph


getRandomString :: StdGen -> Int -> [Char]
getRandomString _ 0 = []
getRandomString g n = do
    let (g1, g2) = split $ g
    (++) (take 1 . randomRs ('a', 'z') $ g1) (getRandomString g2 (n-1))



randomGraph :: IO TickingGraph
randomGraph = do 
    g <- newStdGen
    return . TickingGraph [] $ 0








