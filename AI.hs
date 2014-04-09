{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
--import Data.Array
module AI (Game(..),Heuristic(..),humanPlayer,minimaxPlayer',minimaxPlayer,alphaBetaPlayer,runGame) where
import Data.List
import Control.Applicative
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.Maybe
import GameTrie

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

argmaxM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m a
argmaxM f (x:xs) = _argmaxByM (>) f xs $ f x >>= \q -> return (x,q)


_argmaxByM :: (Monad m) => (b -> b -> Bool) -> (a -> m b) -> [a] -> m (a, b) -> m a
_argmaxByM isBetterThan f = go
	where
	go [] p = p >>= return . fst
	go (x:xs) p = go xs $! cmp x p

	cmp x p = do
		fx <- f x
		(y,fy) <- p
		if isBetterThan fx fy then return (x,fx)
		else return (y,fy)

safeLog :: Float -> Float
safeLog x = if x==0.0 then 0 else log x


class (Eq g, Eq m, Read m, Show m, Read g, Show g) => Game g m | g -> m where
	moveList :: g -> [m]
	moveList' :: g -> [(g,m)]
	gameOver :: g -> Bool
	seedOpts :: g -> [(g,Float)]
	makeMoveNoSeed :: g -> m -> g
	seed :: g -> IO g
	makeMove :: g -> m -> IO g
	newBoard :: IO g

data Heuristic g = Heuristic (g -> Float)
runH :: Heuristic g -> g -> Float
runH (Heuristic h) board = h board

data Player g m where
	MakePlayer :: Game g m => (g -> IO m) -> Player g m
	MakeStatefulPlayer :: Game g m => (g -> InnerState g) -> (g -> IOGameState g m) -> Player g m

checkMoveList :: Game g m => g -> GameState g [g]
--checkMoveList xs = flip fmap getChildren $ \children -> \xs -> if null xs then map snd $ f children else xs
checkMoveList board = do
	empty <- checkEmpty
	p <- onPoint board
	if empty then do
		if p then do
			insertAll $ map fst $ moveList' board
			return $ map fst $ moveList' board
		else return $ map fst $ moveList' board
	else getChildren

checkMoveList' :: Game g m => g -> GameState g [GameState g g]
checkMoveList' board = do
	empty <- checkEmpty
	p <- onPoint board
	if empty then do
		if p then do
			insertAll $ map fst $ moveList' board
			return $ map (return . fst) $ moveList' board
		else return $ map (return . fst) $ moveList' board
	else getChildren'

checkSeedOpts :: Game g m => g -> GameState g [(g,Float)]
checkSeedOpts board = let moves = seedOpts board in do
	empty <- checkEmpty
	--p <- onPoint board
	if empty then do
		insertAll $ map fst moves
		return moves
	else return moves

checkMoveOpts :: Game g m => g -> GameState g [(g,m)]
checkMoveOpts board = let moves = moveList' board in do
	empty <- checkEmpty
	p <- onPoint board
	if empty && p then do
		insertAll $ map fst moves
		return moves
	else return moves

humanPlayer :: Game g m => Player g m
humanPlayer = MakePlayer $ \board -> do {putStrLn (show board); putStr "move: "; line <- getLine; return $ read line}

minimax :: Game g m => g -> Int -> Bool -> Heuristic g -> Float
minimax board depth _ h | depth == 0 || gameOver board = runH h board
minimax board depth True h = maximum $ map (\(x,_) -> minimax x (depth-1) False h) (moveList' board)
minimax board depth False h = sum $ map (\(board',p) -> (*p) $ minimax board' (depth-1) True h) (seedOpts board) 


minimax' :: Game g m => g -> Int -> Bool -> Heuristic g -> GameState g Float
minimax' board depth _ h | depth == 0 || gameOver board = modify up >> (return $ runH h board)
minimax' board depth True h = do
	moves <- checkMoveList board
	ans <- mapM (\x -> softDown x >> minimax' x (depth-1) False h) moves
	modify up
	return $ maximum ans
minimax' board depth False h = do
	moves <- checkSeedOpts board
	ans <- mapM (\(board',p) -> fmap (*p) $ (softDown board' >> minimax' board' (depth-1) True h)) moves
	modify up
	return $ sum ans
 --fmap (maximum . (map (\x -> minimax' x (depth-1) False h))) (checkMoveList board moveList')


alphaBeta :: Game g m => g -> Int -> Bool -> Float -> Float -> Heuristic g -> Float
alphaBeta board depth _ _ _ h | depth == 0 || gameOver board = runH h board
alphaBeta board depth True a b h = helper a  $ sortBy (\x-> \y -> compare (runH h x) (runH h y)) $ map fst $ moveList' board
	where
	--helper :: Game g m => Float -> [g] -> Float
	helper a' [] = a'
	helper a' (x:xs) = let y = max a' $ alphaBeta x (depth-1) False a' b h in if b <= y then y else helper y xs
alphaBeta board depth False a b h 
	| length (seedOpts board) < 10 = helper (seedOpts board) 0.0
	| otherwise = helper' a  $ sortBy (\x-> \y -> compare (runH h x) (runH h y)) $ map fst $ seedOpts board
	where
	helper [] t = t
	helper ((x,p):xs) t = helper xs $ (+t) . (*p) $ alphaBeta x (depth-1) True a b h
	helper' b' [] = b'
	helper' b' (x:xs) = let y = min b' $ alphaBeta x (depth-1) True a b' h in if y <= a then y else helper' y xs

minimaxPlayer :: Game g m => Int -> Heuristic g -> Player g m
minimaxPlayer n h = MakePlayer $ \board -> do {return $ argmax (\x -> minimax (makeMoveNoSeed board x) n False h) (moveList board) }

minimaxPlayer' :: Game g m => Int -> Int -> Heuristic g -> Player g m
minimaxPlayer' m n h = MakeStatefulPlayer (initialState m) $ \board -> do
	moves <- liftGame $ checkMoveOpts board
	move <- liftGame $ fmap snd $ argmaxM (\(x,_) -> softDown x >> minimax' x n False h) moves
	liftGame $ down (makeMoveNoSeed board move)
	return move

alphaBetaPlayer :: Game g m => Int -> Heuristic g -> Player g m
alphaBetaPlayer n h = MakePlayer $ \board -> return $ argmax (\x -> alphaBeta (makeMoveNoSeed board x) n False (-1.0/0.0) (1.0/0.0) h) (moveList board)

runGame :: Player g m -> IO ()
runGame (MakePlayer f) = newBoard >>= step
	where
	--step :: g -> IO ()
	step board | gameOver board = putStrLn $ show board
	step board | otherwise = do
		move <- f board
		board' <- makeMove board move
		putStrLn $ show move
		putStrLn $ show board'
		step board'
runGame (MakeStatefulPlayer init f) = do
	board <- newBoard
	putStrLn $ show board
	evalStateT (step board) (init board)
	where
	-- step :: Game g m => s -> (g -> StateT s IO m) -> IO ()
	step board | gameOver board = lift . putStrLn $ show board
	step board = do
		move <- f board
		board' <- lift $ makeMove board move
		lift . putStrLn $ show move
		lift . putStrLn $ show board'
		liftGame $ down board'
		--lift . putStrLn . show =<< liftGame (onPoint board')
		step board'

























