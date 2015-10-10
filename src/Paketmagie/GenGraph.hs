module Paketmagie.GenGraph where

import System.Random
import Paketmagie.Graph
import System.Environment (getArgs)


getRandomString :: StdGen -> Int -> String
getRandomString _ 0 = []
getRandomString g n = do
    let (g1, g2) = split $ g
    (++) (take 1 . randomRs ('a', 'z') $ g1) (getRandomString g2 (n-1))


-- | a Random number generator and how many elements the List should contain
genNodes :: StdGen -> Int -> [Node]
genNodes _ 0 = []
genNodes g n = do 
    let (g1, g2) = split $ g
    (:) (getRandomString g1 12) (genNodes g2 (n-1))


genEdges :: StdGen -> Int -> [Node] -> [Edge]
genEdges g n nodes  
    | n == length nodes - 1 = []
    | nodes == [] = []
    | otherwise = 
    let (g1, g2) = split $ g
        [n1, n2, n3] = take 3 . randomRs (0, length nodes - 1) $ g1
        node = nodes !! n
        edge1 = Edge node (nodes !! n1) (genProbs g 96)
        edge2 = Edge node (nodes !! n2) (genProbs g1 96)
        edge3 = Edge node (nodes !! n3) (genProbs g2 96)
    in edge1:edge2:edge3:(genEdges g2 (n+1) nodes) 
    


genProbs :: StdGen -> Int -> [Double]
genProbs _ 0 = []
genProbs g n = do 
    let (g1, g2) = split $ g
    (:) (fst . randomR (0.5, 1.0) $ g1) (genProbs g2 (n-1))


randomGraph :: IO TickingGraph
randomGraph = do 
    g <- newStdGen
--    n <- getArgs
    let n = 50
    let nodeList = genNodes g n
        edgeList = genEdges g 0 nodeList

    return . TickingGraph edgeList $ 0



-- removeDoubleEdges :: [Edge] -> 




