-- {-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphs where

import           Control.Monad   (unless)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Aeson      (FromJSON, ToJSON)

type Tick = Integer

type Id = String
type Node = Id

data Edge = Edge Node Node [Float]
    deriving (Show)

data Graph = Graph [Edge]
  deriving (Show)

type Path = [Node]

type Tickable = M.Map (Node, Node) [Float]

squash :: Tickable -> Tickable
squash = fmap f
    where f (a:b:xs) = a + b : xs
          f rest = rest

getProb :: (Node, Node) -> Tickable -> Float
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

probabilityLookup :: Tick -> M.Map Tick Float -> Float
probabilityLookup t = fromMaybe 0 . M.lookup t

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

edgesFrom :: Graph -> Node -> [Edge]
edgesFrom (Graph edgeList) node = filter (\(Edge n _ _) -> n == node) edgeList

run :: Node -> Node -> Graph -> Tick -> [(Path, Float, Tick)]
run start end graph maxTicks = go start 1 maxTicks . toTickable $ graph
    where go :: Node -> Float -> Tick -> Tickable -> [(Path, Float, Tick)]
          go _ _ x _ | x <= -1 = []
          go current prob ticks _ | current == end = [([current], prob, maxTicks - ticks)]
          go current prob ticks tickable = let nextTicks = ticks - 1
                                               squashed = squash tickable
            in go current prob nextTicks squashed ++ nexts current prob nextTicks tickable

          nexts :: Node -> Float -> Tick -> Tickable -> [(Path, Float, Tick)]
          nexts current prob ticks tickable = filter (\(_, prob, _) -> prob /= 0.0) . map (prepend current) . concatMap launch . edgesFrom graph $ current
            where launch (Edge current next _) = go next (prob * getProb (current, next) tickable) ticks (tick tickable)

          prepend :: a -> ([a], b, c) -> ([a], b, c)
          prepend x (xs, y, z) = (x:xs, y, z)

pathSummary :: (Path, Float, Tick) -> IO ()
pathSummary (path, prob, ticks) = do
  unless (null path) $ return ()
  putStrLn showString
  where showString = route ++ " Ticks: " ++ show ticks ++ " Propability: " ++ show prob
        route      = if null path then "" else foldl1 (\folded el -> folded ++ " -> " ++ el) path

calcRoute :: Node -> Node -> IO ()
calcRoute node1 node2 = mapM_ pathSummary $ run node1 node2 exampleGraph 4
