import Data.List (nub)

type Coordinate = (Int, Int)
type ChessBoard = [[Int]]


chessBoardSize :: Int
chessBoardSize = 8


initialPosition :: Coordinate
initialPosition = (0, 0)


isWithinBoard :: Coordinate -> Bool
isWithinBoard (x, y) = x >= 0 && y >= 0 && x < chessBoardSize && y < chessBoardSize


isUnvisited :: ChessBoard -> Coordinate -> Bool
isUnvisited chessBoard (x, y) = chessBoard !! x !! y == 0


allChessBoards :: [ChessBoard]
allChessBoards = replicate chessBoardSize <$> sequence (replicate chessBoardSize [0..chessBoardSize * chessBoardSize - 1])


knightMoves :: ChessBoard -> Coordinate -> [Coordinate]
knightMoves chessBoard (x, y) = filter (\coord -> isWithinBoard coord && isUnvisited chessBoard coord) allCoords
  where
    moveOptions = [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]
    allCoords = nub [ (x + dx, y + dy) | (dx, dy) <- moveOptions, let (newX, newY) = (x + dx, y + dy)]


knightTourPath :: ChessBoard -> Coordinate -> Int -> Maybe ChessBoard
knightTourPath chessBoard pos@(x, y) stepCount
  | stepCount == chessBoardSize * chessBoardSize = Just chessBoard
  | otherwise =
    let possibleNextMoves = knightMoves chessBoard pos
        nextBoards = [knightTourPath (applyMove chessBoard move stepCount) move (stepCount + 1) | move <- possibleNextMoves]
    in case filter (/= Nothing) nextBoards of
        [] -> Nothing
        (Just solution:_) -> Just solution


applyMove :: ChessBoard -> Coordinate -> Int -> ChessBoard
applyMove chessBoard (x, y) stepCount = 
  let modifiedRow = replaceElement y stepCount (chessBoard !! x)
      replaceElement n newVal (x:xs)
         | n == 0 = newVal:xs
         | otherwise = x : replaceElement (n-1) newVal xs
      replaceElement _ _ [] = []
  in take x chessBoard ++ [modifiedRow] ++ drop (x + 1) chessBoard


findKnightTourPath :: Maybe ChessBoard
findKnightTourPath = knightTourPath (replicate chessBoardSize (replicate chessBoardSize 0)) initialPosition 1


main :: IO ()
main = do
  let tourResult = findKnightTourPath
  case tourResult of
    Just chessBoard -> mapM_ print chessBoard
    Nothing -> putStrLn "No solution found."