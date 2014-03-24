--import Data.Array
import Data.Array.IArray
import System.Random

argmax                :: (Ord b) => (a -> b) -> [a] -> a
argmax f (x:xs) = _argmaxBy (>) f xs (x, f x)

_argmaxBy :: (b -> b -> Bool) -> (a -> b) -> [a] -> (a,b) -> a
_argmaxBy isBetterThan f = go
    where
    go []     (b,_)  = b
    go (x:xs) (b,fb) = go xs $! cmp x (b,fb)
    
    cmp a (b,fb) = let  fa = f a in
                   if   fa `isBetterThan` fb
                   then (a,fa)
                   else (b,fb)





data Tile = Tile {val :: Int, merged :: Bool}
	deriving (Read,Eq)
blank :: Tile
blank = Tile {val=0, merged=False}
two :: Tile
two = Tile {val=2, merged=False}
four :: Tile
four = Tile {val=4, merged=False}

pad :: String -> String
pad str | length str < 4 = str ++ (replicate (4 - length str) ' ')
pad str | otherwise = str

instance Show Tile where
	show t | val t == 0 = "----"
	show t = show $ val t

data Move = Up | Down | Lft | Rght | Top | Bottom
	deriving (Show,Enum,Eq,Read)
getDx :: Move -> (Int,Int,Int)
getDx Lft = (0,-1,0)
getDx Rght = (0,1,0)
getDx Up = (-1,0,0)
getDx Down = (1,0,0)
getDx Bottom = (0,0,-1)
getDx Top = (0,0,1)

apply :: (Int,Int,Int) -> Move -> Maybe (Int,Int,Int)
apply (x,y,z) move = let (x2,y2,z2) = getDx move in
	case x+x2<3 && x+x2>=0 && y+y2<3 && y+y2>=0 && z+z2<3 && z+z2>=0 of
		True -> Just (x+x2,y+y2,z+z2)
		False -> Nothing

data Board = Board {
	arr :: (Array (Int,Int,Int) Tile),
	score :: Int
	}
	deriving (Eq)

gameOver :: Board -> Bool
gameOver board = (moveList board) == [] || any (\x-> (val x) == 2048) (map (getTile board) enumBoard)


showArr :: Array (Int,Int,Int) Tile -> String
showArr arr = helper $ map (pad . show . (arr !)) enumBoard --foldl (flip $ (++) . show . (arr !)) "" enumBoard 
	where
	helper :: [String] -> String
	helper (a:b:c:d:e:f:g:h:i:xs) = a ++ b ++ c ++ "\t" ++ d ++ e ++ f ++ "\t" ++ g ++ h ++ i ++ "\n" ++ helper xs
	helper _ = ""

instance Show Board where
	show board = "score: " ++ show (score board) ++ "\n" ++ showArr (arr board)

incScore :: Int -> Board -> Board
incScore n board = Board {arr = arr board, score = (+n) (score board)}

getTile :: Board -> (Int,Int,Int) -> Tile
getTile board ps = (arr board) ! ps

setTile :: Board -> (Int,Int,Int) -> Tile -> Board
setTile board ps tile = Board { arr = (arr board) // [(ps,tile)], score = score board}

setTiles :: Board -> [((Int,Int,Int),Tile)] -> Board
setTiles board xs = Board {arr = (arr board) // xs, score = score board}

incScoreAndSetTiles :: Board -> Int -> [((Int,Int,Int),Tile)] -> Board
incScoreAndSetTiles board n xs = Board {arr = (arr board) // xs, score = (score board) + n}

moveTile :: Board -> (Int,Int,Int) -> (Int,Int,Int) -> Board
moveTile board p1 p2 | p1 == p2 =  board
	|otherwise = setTiles board [(p2, getTile board p1), (p1,blank)] --setTile (setTile board p2 (getTile board p1)) p1 blank

resetTile :: Tile -> Tile
resetTile (Tile v _) = Tile v False

enumBoard :: [(Int,Int,Int)]
enumBoard = [(i,j,k) | i<-[0..2], k<-[2,1,0], j<-[0..2]]

moveEnum :: Move -> [(Int,Int,Int)]
moveEnum Up = [(i,j,k) | i<-[0..2], k<-[0..2], j<-[0..2]]
moveEnum Down = [(i,j,k) | i<-[2,1,0], k<-[0..2], j<-[0..2]]
moveEnum Lft = [(i,j,k) | i<-[0..2], k<-[0..2], j<-[0..2]]
moveEnum Rght = [(i,j,k) | i<-[0..2], k<-[0..2], j<-[2,1,0]]
moveEnum Top = [(i,j,k) | i<-[0..2], k<-[2,1,0], j<-[0..2]]
moveEnum Bottom = [(i,j,k) | i<-[0..2], k<-[0..2], j<-[0..2]]


resetBoard :: Board -> Board
resetBoard board = Board { arr = amap resetTile (arr board), score = score board}

blanks :: Board -> [(Int,Int,Int)]
blanks board = filter (helper board) enumBoard
	where
	helper :: Board -> (Int,Int,Int) -> Bool
	helper board ps = case getTile board ps of
		(Tile 0 _) -> True
		_ -> False

newBoard :: Board
newBoard = Board { arr = array ((0,0,0),(2,2,2)) (map (\x -> (x,blank)) enumBoard), score = 0}

data RandomMove = RandomMove Float [((Int,Int,Int),Tile)]
	deriving (Show,Read,Eq)

seedOpts :: Board -> [RandomMove]
seedOpts board = two' ++ four' ++ twos ++ fours ++ twofour
	where
	two' :: [RandomMove]
	two' = map (\x-> RandomMove (0.48*b1) [(x,two)]) (blanks board)
	four' :: [RandomMove]
	four' = map (\x -> RandomMove (0.16*b1) [(x,four)]) (blanks board)
	twos :: [RandomMove]
	twos = map (\(x,y) -> RandomMove (0.256*b2) [(x,two),(y,two)]) pickTwo
	fours :: [RandomMove]
	fours = map (\(x,y) -> RandomMove (0.016*b2) [(x,four),(y,four)]) pickTwo
	twofour :: [RandomMove]
	twofour = map (\(x,y) -> RandomMove (0.088*b2) [(x,two),(y,four)]) pickTwo
	b1 :: Float
	b1 = 1.0 / (fromIntegral $ length (blanks board))
	b2 :: Float
	b2 = let x=fromIntegral $ length (blanks board) in 1.0/(x*x-x)
	pickTwo :: [((Int,Int,Int),(Int,Int,Int))]
	pickTwo = [(xs,ys) | xs <- blanks board, ys <- blanks board, xs /= ys]

applyRandomMove :: Board -> RandomMove -> Board
applyRandomMove board (RandomMove _ xs) = setTiles board xs
		--foldl helper board xs
	--where
	--helper :: Board -> ((Int,Int,Int),Tile) -> Board
	--helper board (ps,t) = setTile board ps t

seed :: Board -> IO Board
seed board = do
	board' <- addRandom board
	r <- randomRIO (0.0,1.0) :: IO Float
	if (r < 0.4) then (addRandom board') else (return board')
	where
	addRandom :: Board -> IO Board
	addRandom board | blanks board /= [] = do
		ps <- choice $ blanks board
		r <- randomRIO (0.0,1.0) :: IO Float
		return $ setTile board ps (if (r < 0.8) then two else four)
		| otherwise = return board
	--addRandom board = fmap (\x -> setTile board x two) $ choice (blanks board)

choice :: [a] -> IO a
choice xs = fmap (xs !!) $ randomRIO (0, (length xs) - 1)

isValidMove :: Board -> Move -> Bool
isValidMove board move = (makeMoveNoSeed board move) /= board

moveList :: Board -> [Move]
moveList board | any (\x-> (val x) == 2048) (map (getTile board) enumBoard) = []
	| otherwise = filter (isValidMove board) [Up,Down,Lft,Rght,Bottom,Top]

moveList' :: Board -> [(Board,Move)]
moveList' board | any ((==2048) . val) $ map (getTile board) enumBoard = []
	| otherwise = filter (\(x,y) -> x /= board) $ map (\x -> (makeMoveNoSeed board x,x)) [Up,Down,Lft,Rght,Bottom,Top]

makeMoveNoSeed :: Board -> Move -> Board
makeMoveNoSeed board move = resetBoard $ foldl helper board (moveEnum move)
	where
	helper :: Board -> (Int,Int,Int) -> Board
	helper board ps
		| getTile board ps == blank = board
		| (position board ps) /= ps && val (next board ps) == val (getTile board ps) && 
			not (merged (next board ps)) && (getTile board ps) /= blank = 
			let t=Tile {val = (*2) $ val $ next board ps, merged=True } in 
				incScoreAndSetTiles board (val $ getTile board ps) [(position board ps,t),(ps,blank)]  
				--setTile (setTile board (position board ps) t) ps blank
		| otherwise = moveTile board ps $ position board ps
	farthestPosition :: Int -> (Int,Int,Int) -> Board -> Move -> (Int,Int,Int)
	farthestPosition v ps board move = case fmap (getTile board) $ apply ps move of
		Nothing -> ps
		Just (Tile 0 _) -> farthestPosition v (fromJust $ apply ps move) board move
		Just (Tile v' False) -> if v' == v then farthestPosition v (fromJust $ apply ps move) board move else ps
		Just _ -> ps
	position :: Board -> (Int,Int,Int) -> (Int,Int,Int)
	position board ps = farthestPosition (val $ getTile board ps) ps board move
	next :: Board -> (Int,Int,Int) -> Tile
	next board = (getTile board) . (position board)
	fromJust :: Maybe a -> a
	fromJust (Just x) = x

makeMove :: Board -> Move -> IO Board
makeMove board move = seed $ makeMoveNoSeed board move

data Player = Player { getMove :: Board -> IO Move }
humanPlayer :: Player
humanPlayer = Player { getMove = \board -> do {putStrLn (show board); putStr "move: "; line <- getLine; return (read line)} }


type Heuristic = Board -> Float
type Algorithm = Board -> Heuristic -> Int -> IO Move


minimax :: Board -> Int -> Bool -> Heuristic -> Float
minimax board depth _ h | depth == 0 || gameOver board = h board
minimax board depth True h = maximum . ([-1.0/0.0]++) $ map (\(x,_) -> minimax x (depth-1) False h) (moveList' board)
minimax board depth False h = sum $ map (\r@(RandomMove x _) -> (*x) $ minimax (applyRandomMove board r) (depth-1) True h) (seedOpts board) 

alphaBeta :: Board -> Int -> Bool -> Float -> Float -> Heuristic -> Float
alphaBeta board depth _ _ _ h | depth == 0 || gameOver board = h board
alphaBeta board depth True a b h = helper a  $ map (\(x,_) -> x) (moveList' board)
	where
	helper :: Float -> [Board] -> Float
	helper a' [] = a'
	helper a' (x:xs) = let y = alphaBeta x (depth-1) False a' b h in if b <= a' then a' else helper (max a' y) xs
alphaBeta board depth False a b h = helper b $ map (\(x,_) -> x) (moveList' board)
	where
	helper :: Float -> [Board] -> Float
	helper b' [] = b'
	helper b' (x:xs) = let y = alphaBeta x (depth-1) True a b' h in if b' <= a then a else helper (min b' y) xs

h1 :: Heuristic
h1 = fromIntegral . score

-- Source of significant slowdowns
h2 :: Heuristic
h2 board = fromIntegral $ (score board) + (length $ moveList board)

minimaxPlayer :: Int -> Heuristic -> Player
minimaxPlayer n h = Player { getMove = \board -> do {return $ argmax (\x -> minimax (makeMoveNoSeed board x) n True h) (moveList board) } }

alphaBetaPlayer :: Int -> Heuristic -> Player
alphaBetaPlayer n h = Player { getMove = \board -> do {return $ argmax (\x -> alphaBeta (makeMoveNoSeed board x) n True (-1.0/0.0) (1.0/0.0) h) (moveList board) } }


runGame :: Player -> IO ()
runGame player = seed newBoard >>= step player
	where
	step :: Player -> Board -> IO ()
	step player board | gameOver board = return ()
	step player board | otherwise = do
		move <- (getMove player) board
		board' <- makeMove board move
		putStrLn $ show board'
		step player board'

main :: IO ()
main = runGame $ alphaBetaPlayer 10 h1

























