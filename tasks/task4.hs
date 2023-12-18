import Data.List (nub, permutations)


type Network a = [(a, [a])]


allRoutes :: (Eq a) => Network a -> a -> a -> [[a]]
allRoutes network source target = 
  filter (validRoute network source target) $ permutations $ nub $ concatMap (\(node, adjacent) -> node : adjacent) network


validRoute :: (Eq a) => Network a -> a -> a -> [a] -> Bool
validRoute network source target route = 
  head route == source && last route == target && all (\(a, b) -> (a, b) `elem` network || (b, a) `elem` network) (zip route (tail route))


recursiveSearch :: (Eq a) => a -> a -> Network a -> [a] -> [[a]]
recursiveSearch currentNode targetNode network visitedNodes
  | currentNode == targetNode = [visitedNodes ++ [currentNode]]
  | otherwise =
    let adjacentNodes = case lookup currentNode network of
                          Just adj  -> adj
                          Nothing   -> []
        unvisitedNodes = filter (`notElem` visitedNodes) adjacentNodes
        routesFromAdjacents = concatMap (\adjacent -> recursiveSearch adjacent targetNode network (visitedNodes ++ [currentNode])) unvisitedNodes
    in routesFromAdjacents

-- main
main :: IO ()
main = do
  let sampleNetwork = [(1, [2, 3]), (2, [4]), (3, [4]), (4, [5]), (5, [])]
  let initialNode = 1
  let finalNode = 5

  let routes = allRoutes sampleNetwork initialNode finalNode

  putStrLn $ "All routes from " ++ show initialNode ++ " to " ++ show finalNode ++ ":"
  mapM_ print routes