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

instance FromJSON Graph where
  parseJSON (Object v) = Graph <$> v .: "edges"

instance ToJSON Graph where
  toJSON (Graph edges) = object [ "edges" .= edges ]

main :: IO ()
main = do
  jsonIn <- B.getContents
  args   <- getArgs
  let graph :: Maybe Graph
      graph = decode jsonIn

  when (isNothing graph) $ do
    putStrLn "Could not parse graph"
    return ()

  when (length args == 3) $ do
    let startNode = head args
        endNode   = args !! 1
        maxTicks  :: Tick
        maxTicks  = read $ args !! 2
        paths     = sortBy (\(_, p1, _) (_, p2, _) -> compare p2 p1) $ run startNode endNode (fromJust graph) maxTicks
        bestPath  = head paths
--    pathSummary bestPath
--    mapM_ pathSummary paths
    B.putStr $ encode bestPath
