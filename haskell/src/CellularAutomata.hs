module CellularAutomata where

data Cell = Alive | Dead deriving (Eq)
type Grid = [[Cell]]

instance Show Cell where
  show Dead = " "
  show Alive = "X"

-- instance Show List where
  

emptyGrid :: Int -> Int -> Grid
emptyGrid w h = replicate w (replicate h Dead)

flipCell :: Cell -> Cell
flipCell Alive = Dead
flipCell Dead = Alive

getCell :: (Int, Int) -> Grid -> Cell
getCell (x,y) g = g !! x !! y

getNeighs :: (Int, Int) -> Grid -> [Cell]
getNeighs (x,y) g = (`getCell` g) <$> [(x',y') | x' <- [x-1,x,x+1], y' <- [y-1,y,y+1]]

updateGrid :: (Int, Int) -> Grid -> Grid
updateGrid (x,y) g = take x g ++ [newRow] ++ drop (x+1) g
  where
    row = g !! x
    newRow = take y row ++ [flipCell (row !! y)] ++ drop (y+1) row
