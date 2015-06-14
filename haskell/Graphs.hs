module Graphs where

import           Control.Monad   (unless)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

-- | A unit of time, probably 15 minutes
type Tick = Integer

-- | A node is a point on the map, represented by its name
type Node = String

-- | An edge represents a path between two nodes and the probabilities
-- associated with it in a list. Each elemnt of that list corresponds
-- to a tick.
data Edge = Edge Node Node [Float]
    deriving (Show)

-- | A graph is just a list of edges, as nodes don't have any data associated
-- with them other than their identity
data Graph = Graph [Edge]
  deriving (Show)

-- | A path is a list of tuples of nodes and ticks. The nodes represent the
-- waypoints a packet is dropped of and picked up at. The ticks are the
-- aproximated max ticks a packet will rest at that point without being
-- picked up.
type Path = [(Node, Tick)]

-- | A tickable is a structure that represents the changing probabilities over
-- time
type Tickable = (M.Map (Node, Node) [Float], Tick)

-- | Squashing means waiting at a waypoint, hence the probabilities will be
-- added up and the waiting counter is increased.
squash :: Tickable -> Tickable
squash (m, t) = (fmap f m, t + 1)
    where f (a:b:xs) = a + b : xs
          f rest = rest

-- | getProb retrieves the current probability associated with a connection
-- between two nodes
getProb :: (Node, Node) -> Tickable -> Float
getProb a = fromMaybe 0 . safeHead . M.lookup a . fst
    where safeHead (Just []) = Nothing
          safeHead xs        = fmap head xs

-- | tick means a tick passed. It updates the tickable to represent that change.
tick :: Tickable -> Tickable
tick (m, _) = (fmap safeTail m, 0)
    where safeTail [] = []
          safeTail xs = tail xs

-- | waitDuration retrieves the duration a packet has waited at the current
-- waypoint.
waitDuration :: Tickable -> Tick
waitDuration = snd

-- | toTickable converts a graph into a tickable
toTickable :: Graph -> Tickable
toTickable (Graph edges) = (foldr f M.empty edges, 0)
    where f (Edge from to ratings) = M.insert (from, to) ratings

-- | exampleGraph is our test data structure for fiddling around in ghci
exampleGraph :: Graph
exampleGraph = Graph [
    Edge "Königsplatz"  "Theater"      [0.75, 0.50, 0.12, 0.30]
  , Edge "Theater"      "Dom"          [0.32, 0.45, 0.85, 0.63]
  , Edge "Königsplatz"  "Rathausplatz" [0.10, 0.10, 0.10, 0.30]
  , Edge "Rathausplatz" "Moritzplatz"  [0.70, 0.01, 0.50, 0.20]
  , Edge "Rathausplatz" "Dom"          [0.25, 0.10, 0.20, 0.50]
  , Edge "Dom"          "Rathausplatz" [0.30, 0.13, 0.50, 0.60]
  , Edge "Moritzplatz"  "Königsplatz"  [0.20, 0.41, 0.10, 0.03]
    ]

-- | edgesFrom returns all the edges in a graph that point away from a specified
-- node
edgesFrom :: Graph -> Node -> [Edge]
edgesFrom (Graph edgeList) node = filter (\(Edge n _ _) -> n == node) edgeList

-- | run runs the unwieldy algorithm
-- It calculates all possible routes between node within a certain duraition,
-- and returns them, along with their total probability and their duraition
-- in ticks
run :: Node -> Node -> Graph -> Tick -> [(Path, Float, Tick)]
run start end graph maxTicks = go start 1 maxTicks . toTickable $ graph
    where go :: Node -> Float -> Tick -> Tickable -> [(Path, Float, Tick)]
          go _ _ x _ | x <= -1 = []
          go current prob ticks tickable | current == end = [([(current, waitDuration tickable)], prob, maxTicks - ticks)]
          go current prob ticks tickable = let nextTicks = ticks - 1
                                               squashed = squash tickable
            in go current prob nextTicks squashed ++ nexts current prob nextTicks tickable

          nexts :: Node -> Float -> Tick -> Tickable -> [(Path, Float, Tick)]
          nexts current prob ticks tickable = map (prepend (current, waitDuration tickable)) . concatMap launch . edgesFrom graph $ current
            where launch (Edge current next _) = go next (prob * getProb (current, next) tickable) ticks (tick tickable)

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
calcRoute node1 node2 = mapM_ pathSummary . filter zeroProb . run node1 node2 exampleGraph $ 4
    where zeroProb (_, 0.0, _) = False
          zeroProb _           = True
