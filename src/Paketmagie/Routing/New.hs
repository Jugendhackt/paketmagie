module Paketmagie.Routing.New
    ( bestPath
    ) where

import Paketmagie.Graph
import Paketmagie.Routing (Path)

import Data.Function (on)
import Data.List (delete)

-- ** Algorithm Concept **
-- (Run Dijkstra over Graph)
-- Start with initial state
-- Iteratively apply A* until Destination is reached

-- | Weight of a Path
type Weight = Double

-- | The representation of the state in the algorithm
data AlgoState = AlgoState
    { graphState  :: TickingGraph
    , stateWeight :: Weight
    , remainingTicks :: Tick
    , probability :: Probability
    , path        :: [Node]
    } deriving (Eq)

-- | weight calculates the weight of a Path
weight :: Probability -> Tick -> Weight
weight prob tick = prob ** (1 / fromIntegral tick)

-- | bestPath selects a Path with a high chance to be the best possible path,
--   given the ticks constrain
bestPath :: Node -> Node -> TickingGraph -> Tick -> ([Node], Probability, Tick)
bestPath start end graph ticks =
    -- Start with an initial state
    let initialState = AlgoState graph 1.0 ticks 1.0 [start]
    in  iterAlgo [initialState]
    -- iterAlgo is designed to be the A* pathfinding algorithm
    where iterAlgo states =
            -- Progress the state with the highest weight
            let iterState = findByComparison states ((<) `on` stateWeight)
                progressResult = progress iterState
            in  case progressResult of
                    -- Return the result if the destination node was reached
                    Left (path, prob, ticksLeft) ->
                        let duration = ticks - ticksLeft
                        in  (path, prob, duration)
                    -- Otherwise, continue iterating
                    Right nextStates ->
                        -- Replace the progressed state with the states
                        -- following it
                        let newStates = nextStates ++ delete iterState states
                        in iterAlgo states

          -- progress generates the states following a state.
          -- If one of these states reaches the end node, only it is returned,
          -- otherwise, the function returns all of the states.
          progress currentState@(AlgoState graph _ ticks prob path) =
                -- The current node is always at the end of the path
            let currentNode = last path
                edges       = edgesFrom graph currentNode
                -- A list of the states that arrived at the end node
                arrived     = filter (\(Edge _ b _) -> b == end) edges
                nextTick    = ticks - 1
            in  if not . null $ arrived
                    -- A state arrived at the end node
                    then let endProb = getProb (currentNode, end) graph
                         in  Left (end:path, prob * endProb, nextTick)
                    -- All possible states need to be returned
                    else let -- Generate the state representing the action of
                             -- waiting on the current node
                             waitGraph = squash graph
                             waitWeight = weight prob nextTick
                             waitState = currentState {
                                  graphState  = waitGraph
                                , stateWeight = waitWeight
                                }

                             -- Generate the actual following states
                             tickedGrap = tick graph
                             moveStates = flip map edges $ \(Edge _ to (edgeProb:_)) ->
                                let newProb   = prob * edgeProb
                                    newWeight = weight newProb nextTick
                                in AlgoState tickedGrap newWeight nextTick newProb (to:path)
                         in Right $ waitState:moveStates

-- | findByComparison is a helper function for creating functions akin to min
--   and max
findByComparison :: [a] -> (a -> a -> Bool) -> a
findByComparison [ ] _          = error "The list has to be at least of length 1"
findByComparison [x] _          = x
findByComparison (x:xs) compare = findIter x xs
    where findIter best []   = best
          findIter best (x:xs)
            | compare best x = findIter x    xs
            | otherwise      = findIter best xs
