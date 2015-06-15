module Graphs where

import Control.Monad   (unless, join)
import Data.Maybe      (fromMaybe)

-- | A unit of time, probably 15 minutes
type Tick = Integer

-- | A node is a point on the map, represented by its name
type Node = String

-- | An edge represents a path between two nodes and the probabilities
-- associated with it in a list. Each elemnt of that list corresponds
-- to a tick.
data Edge = Edge Node Node [Float]
    deriving (Show)

-- | A ticking graph is a graph whichs edges a changing weights, in this
-- case probabilities. For convenience, it also stores the amount of ticks a
-- packet has waited at a specific waypoint.
data TickingGraph = TickingGraph [Edge] Tick
  deriving (Show)

-- | Squashing means waiting at a waypoint, hence the probabilities will be
-- added up and the waiting counter is increased.
squash :: TickingGraph -> TickingGraph
squash (TickingGraph edges waited) = TickingGraph (map g edges) (waited + 1)
    where f (a:b:xs) = a + b : xs
          f rest = rest
          g (Edge from to probs) = Edge from to . f $ probs

-- | getProb retrieves the current probability associated with a connection
-- between two nodes
getProb :: (Node, Node) -> TickingGraph -> Float
getProb (from, to) (TickingGraph edges _) = fromMaybe 0 . join . fmap probs . safeHead . filter qualifies $ edges
    where safeHead [] = Nothing
          safeHead xs = Just . head $ xs

          qualifies (Edge f t _) | f == from && t == to = True
          qualifies _ = False

          probs (Edge _ _ ps) = safeHead ps

-- | tick means a tick passed. It updates the ticking graph to represent that
-- change.
tick :: TickingGraph -> TickingGraph
tick (TickingGraph edges _) = TickingGraph ticked 0
    where ticked = map (\(Edge f t probs) -> Edge f t . safeTail $ probs) edges
          safeTail [] = []
          safeTail xs = tail xs

-- | waitDuration retrieves the duration a packet has waited at the current
-- waypoint.
waitDuration :: TickingGraph -> Tick
waitDuration (TickingGraph _ t) = t

-- | A path is a list of tuples of nodes and ticks. The nodes represent the
-- waypoints a packet is dropped of and picked up at. The ticks are the
-- aproximated max ticks a packet will rest at that point without being
-- picked up.
type Path = [(Node, Tick)]

-- | exampleGraph is our test data structure for fiddling around in ghci
exampleGraph :: TickingGraph
exampleGraph = TickingGraph [
    Edge "Königsplatz"  "Theater"      [0.75, 0.50, 0.12, 0.30]
  , Edge "Theater"      "Dom"          [0.32, 0.45, 0.85, 0.63]
  , Edge "Königsplatz"  "Rathausplatz" [0.10, 0.10, 0.10, 0.30]
  , Edge "Rathausplatz" "Moritzplatz"  [0.70, 0.01, 0.50, 0.20]
  , Edge "Rathausplatz" "Dom"          [0.25, 0.10, 0.20, 0.50]
  , Edge "Dom"          "Rathausplatz" [0.30, 0.13, 0.50, 0.60]
  , Edge "Moritzplatz"  "Königsplatz"  [0.20, 0.41, 0.10, 0.03]
    ] 0

-- | edgesFrom returns all the edges in a graph that point away from a specified
-- node
edgesFrom :: TickingGraph -> Node -> [Edge]
edgesFrom (TickingGraph edgeList _) node = filter (\(Edge n _ _) -> n == node) edgeList

-- | runAlgorithm runs the unwieldy algorithm
-- It calculates all possible routes between node within a certain duraition,
-- and returns them, along with their total probability and their duraition
-- in ticks
runAlgorithm :: Node -> Node -> TickingGraph -> Tick -> [(Path, Float, Tick)]
runAlgorithm start end graph maxTicks = go start 1 maxTicks $ graph
    where go :: Node -> Float -> Tick -> TickingGraph -> [(Path, Float, Tick)]
          go _ _ x _ | x <= -1 = []
          go current prob ticks graph | current == end = [([(current, waitDuration graph)], prob, maxTicks - ticks)]
          go current prob ticks graph = let nextTicks = ticks - 1
                                            squashed = squash graph
            in go current prob nextTicks squashed ++ nexts current prob nextTicks graph

          nexts :: Node -> Float -> Tick -> TickingGraph -> [(Path, Float, Tick)]
          nexts current prob ticks graph = map (prepend (current, waitDuration graph)) . concatMap launch . edgesFrom graph $ current
            where launch (Edge current next _) = go next (prob * getProb (current, next) graph) ticks (tick graph)

          prepend :: a -> ([a], b, c) -> ([a], b, c)
          prepend x (xs, y, z) = (x:xs, y, z)

-- | pathSummary is a pretty-printer for the output of run
pathSummary :: (Path, Float, Tick) -> IO ()
pathSummary (path, prob, ticks) = do
  unless (null path) $ return ()
  putStrLn showString
  where showString = route path ++ " Ticks: " ++ show ticks ++ " Propability: " ++ show prob

        route = foldr f ""
        f n "" = pathSegment n
        f n st = pathSegment n ++ " -> " ++ st

        pathSegment (node, 0) = node
        pathSegment (node, wait) = node ++ " [" ++ show wait ++ "]"

-- | calcRoute is our testing-function to make sure everything works the way
-- it's supoosed to
calcRoute :: Node -> Node -> IO ()
calcRoute node1 node2 = mapM_ pathSummary . filter zeroProb . runAlgorithm node1 node2 exampleGraph $ 4
    where zeroProb (_, 0.0, _) = False
          zeroProb _           = True
