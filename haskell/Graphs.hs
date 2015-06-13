-- {-# LANGUAGE PatternGuards #-}

module Graphs where

import           Control.Monad   (unless)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

type Tick = Integer

type Id = String
type Node = Id

data Edge = Edge Node Node [Double]
    deriving (Show)

data Graph = Graph [Edge]
  deriving (Show)

type Path = [Node]

type Tickable = M.Map (Node, Node) [Double]

squash :: Tickable -> Tickable
squash = fmap f
    where f (a:b:xs) = a + b : xs
          f rest = rest

getProb :: (Node, Node) -> Tickable -> Double
getProb a b = fromMaybe 0 . safeHead . M.lookup a $ b
    where safeHead (Just []) = Nothing
          safeHead xs        = fmap head xs

tick :: Tickable -> Tickable
tick = fmap safeTail
    where safeTail [] = []
          safeTail xs = tail xs

toTickable :: Graph -> Tickable
toTickable (Graph edges) = foldr f M.empty edges
    where f (Edge from to ratings) = M.insert (from, to) ratings

probabilityLookup :: Tick -> M.Map Tick Double -> Double
probabilityLookup t = fromMaybe 0 . M.lookup t

exampleGraph :: Graph
exampleGraph = Graph [
    Edge "Rathausplatz" "Moritzplatz" [0.7, 0.01, 0.5, 0.2]
  , Edge "Königsplatz" "Dom" [1, 0.1, 0.2, 0.5]
  , Edge "Moritzplatz" "Königsplatz" [0.2, 0.41, 0.1, 0.03]
    ]

edgesFrom :: Graph -> Node -> [Edge]
edgesFrom (Graph edgeList) node = filter (\(Edge n _ _) -> n == node) edgeList

run :: Node -> Node -> Graph -> Tick -> [(Path, Double, Tick)]
run start end graph maxTicks = go start 1 maxTicks . toTickable $ graph
    where go :: Node -> Double -> Tick -> Tickable -> [(Path, Double, Tick)]
          go _ _ x _ | x <= -1 = []
          go current prob ticks _ | current == end = [([current], prob, maxTicks - ticks)]
          go current prob ticks tickable = let nextTicks = ticks - 1
                                               squashed = squash tickable
            in go current prob nextTicks squashed ++ nexts current prob nextTicks tickable

          nexts :: Node -> Double -> Tick -> Tickable -> [(Path, Double, Tick)]
          nexts current prob ticks tickable = map (prepend current) . concatMap launch . edgesFrom graph $ current
            where launch (Edge current next _) = go next (prob * getProb (current, next) tickable) ticks (tick tickable)

          prepend :: a -> ([a], b, c) -> ([a], b, c)
          prepend x (xs, y, z) = (x:xs, y, z)
