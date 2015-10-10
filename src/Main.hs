import           Paketmagie.Graph
import           Paketmagie.Routing

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero, when)
import           Data.ByteString.Lazy as B (getContents, putStr, readFile)
import           Data.List            (sortBy)
import           Data.Aeson
import           Data.Maybe           (fromJust, isNothing)
import           Data.Vector          (Vector, (!))
import           System.Environment   (getArgs)

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
