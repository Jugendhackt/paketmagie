module Paketmagie.Routing where

import           Paketmagie.Graph

import           Control.Monad (join, unless)
import           Data.Maybe    (fromMaybe)

-- | A path is a list of tuples of nodes and ticks. The nodes represent the
-- waypoints a packet is dropped of and picked up at. The ticks are the
-- aproximated max ticks a packet will rest at that point without being
-- picked up.
type Path = [(Node, Tick)]

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

