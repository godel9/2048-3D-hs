--import Data.Array
{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where
import Data.Array.IArray
import System.Random
import Data.Ord
import Data.List
import AI

safeLog :: Float -> Float
safeLog x = if x==0.0 then 0 else log x



data Tile = Tile {val :: Int, merged :: Bool}
	deriving (Read,Eq)
blank :: Tile
blank = Tile {val=0, merged=False}
two :: Tile
two = Tile {val=2, merged=False}
four :: Tile
four = Tile {val=4, merged=False}

pad :: String -> String
pad str | length str < 5 = str ++ (replicate (5 - length str) ' ')
pad str | otherwise = str

instance Show Tile where
	show t | val t == 0 = "-----"
	show t = show $ val t

data Move = Up | Down | Lft | Rght
	deriving (Show,Enum,Eq,Read)
getDx :: Move -> Position
getDx Lft = (0,-1)
getDx Rght = (0,1)
getDx Up = (-1,0)
getDx Down = (1,0)

type Position = (Int,Int)

apply :: Position -> Move -> Maybe Position
apply (x,y) move = let (x2,y2) = getDx move in
	case x+x2<4 && x+x2>=0 && y+y2<4 && y+y2>=0 of
		True -> Just (x+x2,y+y2)
		False -> Nothing

data Board = Board {
	arr :: (Array Position Tile),
	score :: Int
	}
	deriving (Eq,Read)

showArr :: Array Position Tile -> String
showArr arr = helper $ map (pad . show . (arr !)) enumBoard --foldl (flip $ (++) . show . (arr !)) "" enumBoard 
	where
	helper :: [String] -> String
	helper (a:b:c:d:xs) = a ++ b ++ c ++ d ++ "\n" ++ helper xs
	helper _ = ""

instance Show Board where
	show board = "score: " ++ show (score board) ++ "\n" ++ showArr (arr board)

incScore :: Int -> Board -> Board
incScore n board = Board {arr = arr board, score = (+n) (score board)}

getTile :: Board -> Position -> Tile
getTile board ps = (arr board) ! ps

setTile :: Board -> Position -> Tile -> Board
setTile board ps tile = Board { arr = (arr board) // [(ps,tile)], score = score board}

setTiles :: Board -> [(Position,Tile)] -> Board
setTiles board xs = Board {arr = (arr board) // xs, score = score board}

incScoreAndSetTiles :: Board -> Int -> [(Position,Tile)] -> Board
incScoreAndSetTiles board n xs = Board {arr = (arr board) // xs, score = (score board) + n}

moveTile :: Board -> Position -> Position -> Board
moveTile board p1 p2 | p1 == p2 =  board
	|otherwise = setTiles board [(p2, getTile board p1), (p1,blank)] --setTile (setTile board p2 (getTile board p1)) p1 blank

resetTile :: Tile -> Tile
resetTile (Tile v _) = Tile v False

enumBoard :: [Position]
enumBoard = [(i,j) | i<-[0..3], j<-[0..3]]

rowsAndCols :: [[Position]]
rowsAndCols = [[(i,j) | i<-[0..3]] | j<-[0..3]] ++ [[(i,j) | j<-[0..3]] | i<-[0..3]]

moveEnum :: Move -> [Position]
moveEnum Up = [(i,j) | i<-[0..3], j<-[0..3]]
moveEnum Down = [(i,j) | i<-[3,2,1,0], j<-[0..3]]
moveEnum Lft = [(i,j) | i<-[0..3], j<-[0..3]]
moveEnum Rght = [(i,j) | i<-[0..3], j<-[3,2,1,0]]


resetBoard :: Board -> Board
resetBoard board = Board { arr = amap resetTile (arr board), score = score board}

blanks :: Board -> [Position]
blanks board = filter (helper board) enumBoard
	where
	helper :: Board -> Position -> Bool
	helper board ps = case getTile board ps of
		(Tile 0 _) -> True
		_ -> False

choice :: [a] -> IO a
choice xs = fmap (xs !!) $ randomRIO (0, (length xs) - 1)

isValidMove :: Board -> Move -> Bool
isValidMove board move = (makeMoveNoSeed board move) /= board
	
instance Game Board Move where
	gameOver board = any (\x-> (val x) == 2048) (map (getTile board) enumBoard) || (not $ any (\(x:xs) -> helper x xs) lines)
		where
		lines :: [[Int]]
		lines = map (map (val . (getTile board))) rowsAndCols
		helper :: Int -> [Int] -> Bool
		helper _ [] = False
		helper 0 _ = True
		helper a (x:xs) = (a == x) || (helper x xs)
	
	seed board | blanks board /= [] = do
		ps <- choice $ blanks board
		r <- randomRIO (0.0,1.0) :: IO Float
		return $ setTile board ps (if (r < 0.8) then two else four)
		| otherwise = return board
	
	seedOpts board = two' ++ four'
		where
		two' :: [(Board,Float)]
		two' = map (\x-> (setTile board x two, 0.9*b1)) (blanks board)
		four' :: [(Board,Float)]
		four' = map (\x -> (setTile board x four, 0.1*b1)) (blanks board)
		b1 :: Float
		b1 = 1.0 / (fromIntegral $ length (blanks board))

	moveList board | any (\x-> (val x) == 2048) (map (getTile board) enumBoard) = []
		| otherwise = filter (isValidMove board) [Up,Down,Lft,Rght]

	moveList' board | any ((==2048) . val) $ map (getTile board) enumBoard = []
		| otherwise = filter (\(x,_) -> x /= board) $ map (\x -> (makeMoveNoSeed board x,x)) [Up,Down,Lft,Rght]

	makeMoveNoSeed board move = resetBoard $ foldl helper board $ moveEnum move
		where
		helper :: Board -> Position -> Board
		helper board ps
			| getTile board ps == blank = board
			| (position board ps) /= ps && val (next board ps) == val (getTile board ps) && 
				not (merged (next board ps)) && (getTile board ps) /= blank = 
				let t=Tile {val = (*2) $ val $ next board ps, merged=True } in 
					incScoreAndSetTiles board ((*2) . val $ getTile board ps) [(position board ps,t),(ps,blank)]  
				--setTile (setTile board (position board ps) t) ps blank
			| otherwise = moveTile board ps $ position board ps
		farthestPosition :: Int -> Position -> Board -> Move -> Position
		farthestPosition v ps board move = case fmap (getTile board) $ apply ps move of
			Nothing -> ps
			Just (Tile 0 _) -> farthestPosition v (fromJust $ apply ps move) board move
			Just (Tile v' False) -> if v' == v then farthestPosition v (fromJust $ apply ps move) board move else ps
			Just _ -> ps
		position :: Board -> Position -> Position
		position board ps = farthestPosition (val $ getTile board ps) ps board move
		next :: Board -> Position -> Tile
		next board = (getTile board) . (position board)
		fromJust :: Maybe a -> a
		fromJust (Just x) = x

	makeMove board move = seed $ makeMoveNoSeed board move

	newBoard = seed $ Board { arr = array ((0,0),(3,3)) (map (\x -> (x,blank)) enumBoard), score = 0}

h1 :: Heuristic Board
h1 = Heuristic $ fromIntegral . score
	
	
	
main :: IO ()
main = do
	setStdGen $ mkStdGen 1000
	runGame $ minimaxPlayer' 100000 3 h1

























