module Paketmagie.GenGraph
  (randomGraph) where

import           Debug.Trace
import           Paketmagie.Graph
import           System.Environment (getArgs)
import           System.Random

-- | Generates a random string from a given RandomGen and an Int
getRandomString :: (RandomGen a) => a -> Int -> String
getRandomString _ 0 = []
getRandomString g n = do
    let (g1, g2) = split $ g
    (++) (take 1 . randomRs ('a', 'z') $ g1) (getRandomString g2 (n-1))


-- | Returns a list of random nodes of the given length
genNodes :: (RandomGen a) => a -> Int -> [Node]
genNodes _ 0 = []
genNodes g n = do
    let (g1, g2) = split $ g
    (:) (getRandomString g1 12) (genNodes g2 (n-1))


-- | Returns a random list of edges between the given Nodes
genEdges :: (RandomGen a) => a -> [Node] -> [Edge]
genEdges = doGen 0
  where doGen n g nodes
          | n == length nodes - 1 = []
          | nodes == [] = []
          | otherwise =
          let (g1, g2) = split $ g
              [n1, n2, n3] = take 3 . randomRs (0, length nodes - 1) $ g1
              node = nodes !! n
              ticksNum = 30
              edge1 = Edge node (nodes !! n1) (genProbs g ticksNum)
              edge2 = Edge node (nodes !! n2) (genProbs g1 ticksNum)
              edge3 = Edge node (nodes !! n3) (genProbs g2 ticksNum)
          in edge1:edge2:edge3:(doGen (n+1) g2 nodes)



-- | Returns a list of Probabilites of length n generated with g
genProbs :: (RandomGen a) => a -> Int -> [Probabilites]
genProbs _ 0 = []
genProbs g n = do
    let (g1, g2) = split $ g
    (:) (fst . randomR (0.5, 1.0) $ g1) (genProbs g2 (n-1))


-- | Returns a random Graph of length n
randomGraph :: Int -> IO TickingGraph
randomGraph n = do
    g <- newStdGen
    let nodeList = genNodes g n
        edgeList = genEdges g nodeList
    return . TickingGraph (removeDuplicateEdges edgeList) $ 0


-- | Removes duplicate or self-referential edges
removeDuplicateEdges :: [Edge] -> [Edge]
removeDuplicateEdges [] = []
removeDuplicateEdges (x:xs) | selfReferential x = removeDuplicateEdges xs
removeDuplicateEdges (x:xs) =
    x:(removeDuplicateEdges (filter (not . isDroppable x) xs ) )


-- | Triple-matching the edges for duplicates
matchTriple :: [Edge] -> [Edge]
matchTriple [] = []
matchTriple (a:b:c:xs) = let ys = [a, b, c]
                in removeDuplicateEdges ys ++ matchTriple xs


-- | Decides wether an Edge is to drop by checking for duplicates
isDroppable :: Edge -> Edge -> Bool
isDroppable (Edge from1 to1 _) (Edge from2 to2 _) =
    ((from1 == from2) && (to1 == to2) )


-- | Returns wether an Edge is self-referential
selfReferential :: Edge -> Bool
selfReferential (Edge from to _) = from == to
