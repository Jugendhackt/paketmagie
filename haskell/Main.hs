{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative  ((<$>), (<*>))
import           Data.List            (sortBy)
import           Control.Monad        (mzero, when)
import           Data.Aeson
import           Data.ByteString.Lazy as B (readFile, getContents, putStr)
import           Data.Vector          (Vector, (!))
import           Graphs
import           System.Environment   (getArgs)
import Data.Maybe (isNothing, fromJust)

instance FromJSON Edge where
  parseJSON (Object v) = Edge <$>
                         v .: "from" <*>
                         v .: "to"   <*>
                         v .: "propabilities"
  parseJSON _ = mzero

instance ToJSON Edge where
  toJSON (Edge from to props) = object
    [ "from" .= from
    , "to"   .= to
    , "propabilities" .= props
    ]

instance FromJSON TickingGraph where
  parseJSON (Object v) = TickingGraph <$> v .: "edges" <*> v .: "tick"

instance ToJSON TickingGraph where
  toJSON (TickingGraph edges tick) = object [ "edges" .= edges, "tick" .= tick ]

main :: IO ()
main = do
  jsonIn <- B.getContents
  args   <- getArgs
  let graph :: Maybe TickingGraph
      graph = decode jsonIn

  when (isNothing graph) $ do
    putStrLn "Could not parse graph"
    return ()

  when (length args == 3) $ do
    let startNode = head args
        endNode   = args !! 1
        maxTicks  :: Tick
        maxTicks  = read $ args !! 2
        paths     = sortBy (\(_, p1, _) (_, p2, _) -> compare p2 p1) $ runAlgorithm startNode endNode (fromJust graph) maxTicks
        bestPath  = head paths
--    pathSummary bestPath
--    mapM_ pathSummary paths
    B.putStr $ encode bestPath
