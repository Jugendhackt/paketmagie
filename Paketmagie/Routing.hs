module Paketmagie.Routing where

import           Control.Monad (join, unless)
import           Data.Maybe    (fromMaybe)

-- | A unit of time, probably 15 minutes
type Tick = Integer

-- | A node is a point on the map, represented by its name
type Node = String

-- | An edge represents a path between two nodes and the probabilities
-- associated with it in a list. Each element of that list corresponds
-- to a tick.
data Edge = Edge Node Node [Probability]
    deriving (Show)

-- | A ticking graph is a graph whichs edges a changing weights, in this
-- case probabilities. For convenience, it also stores the amount of ticks a
-- packet has waited at a specific waypoint.
data TickingGraph = TickingGraph [Edge] Tick
  deriving (Show)

-- | Squashing means waiting at a waypoint, hence the probabilities will be
-- added up and the waiting counter is increased.
squash :: TickingGraph -> TickingGraph
squash (TickingGraph edges waited) = TickingGraph (map squashEdge edges) (waited + 1)
    where addProbs (a:b:xs) = a + b * (1 - a) : xs
          addProbs rest = rest
          squashEdge (Edge from to probs) = Edge from to . addProbs $ probs

-- | getProb retrieves the current probability associated with a connection
-- between two nodes
getProb :: (Node, Node) -> TickingGraph -> Probability
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

-- | edgesFrom returns all the edges in a graph that point away from a specified
-- node
edgesFrom :: TickingGraph -> Node -> [Edge]
edgesFrom (TickingGraph edgeList _) node = filter (\(Edge n _ _) -> n == node) edgeList

-- | Probability
type Probability = Double

-- | Weight of a Path
type Weight = Double

-- | weight calculates the weight of a Path
weight :: Probability -> Tick -> Weight
weight prob tick = prob ** (1 / (fromIntegral tick))

-- | runAlgorithm runs the unwieldy algorithm
-- It calculates all possible routes between node within a certain duraition,
-- and returns them, along with their total probability and their duraition
-- in ticks
runAlgorithm :: Node -> Node -> TickingGraph -> Tick -> [(Path, Probability, Tick)]
runAlgorithm start end graph maxTicks = go start 1 maxTicks $ graph
    where go :: Node -> Probability -> Tick -> TickingGraph -> [(Path, Probability, Tick)]
          go _ _ x _ | x <= -1 = []
          go current prob ticks graph | current == end = [([(current, waitDuration graph)], prob, maxTicks - ticks)]
          go current prob ticks graph = let nextTicks = ticks - 1
                                            squashed = squash graph
            in go current prob nextTicks squashed ++ nexts current prob nextTicks graph

          nexts :: Node -> Probability -> Tick -> TickingGraph -> [(Path, Probability, Tick)]
          nexts current prob ticks graph = map (prepend (current, waitDuration graph)) . concatMap launch . edgesFrom graph $ current
            where launch (Edge current next _) = go next (prob * getProb (current, next) graph) ticks (tick graph)

          prepend :: a -> ([a], b, c) -> ([a], b, c)
          prepend x (xs, y, z) = (x:xs, y, z)

-- | pathSummary is a pretty-printer for the output of run
pathSummary :: (Path, Probability, Tick) -> IO ()
pathSummary (path, prob, ticks) = do
  unless (null path) $ return ()
  putStrLn showString
  where showString = route path ++ " Ticks: " ++ show ticks ++ " Propability: " ++ show prob

        route = foldr f ""
        f n "" = pathSegment n
        f n st = pathSegment n ++ " -> " ++ st

        pathSegment (node, 0) = node
        pathSegment (node, wait) = node ++ " [" ++ show wait ++ "]"

