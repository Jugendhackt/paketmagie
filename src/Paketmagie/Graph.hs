{-# LANGUAGE OverloadedStrings #-}

module Paketmagie.Graph where

import           Data.Maybe    (fromMaybe)
import           Data.Aeson
import           Control.Monad (join, mzero)

-- | A unit of time, probably 15 minutes
type Tick = Integer

-- | A node is a point on the map, represented by its name
type Node = String

-- | Probability
type Probability = Double

-- | An edge represents a path between two nodes and the probabilities
-- associated with it in a list. Each element of that list corresponds
-- to a tick.
data Edge = Edge Node Node [Probability]
    deriving (Eq, Show)

-- | A ticking graph is a graph whichs edges a changing weights, in this
-- case probabilities. For convenience, it also stores the amount of ticks a
-- packet has waited at a specific waypoint.
data TickingGraph = TickingGraph [Edge] Tick
  deriving (Eq, Show)

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


-- | edgesFrom returns all the edges in a graph that point away from a specified
-- node
edgesFrom :: TickingGraph -> Node -> [Edge]
edgesFrom (TickingGraph edgeList _) node = filter (\(Edge n _ _) -> n == node) edgeList

instance FromJSON Edge where
  parseJSON (Object v) = Edge <$>
                         v .: "from" <*>
                         v .: "to"   <*>
                         v .: "probabilities"
  parseJSON _ = mzero

instance ToJSON Edge where
  toJSON (Edge from to probs) = object
    [ "from" .= from
    , "to"   .= to
    , "probabilities" .= probs
    ]

instance FromJSON TickingGraph where
  parseJSON (Object v) = TickingGraph <$> v .: "edges" <*> v .: "tick"

instance ToJSON TickingGraph where
  toJSON (TickingGraph edges tick) = object [ "edges" .= edges, "tick" .= tick ]

