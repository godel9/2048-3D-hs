module GameTrie (GameTree, Context, InnerState, GameState, IOGameState, initialState, checkEmpty, getChildren, getChildren', getBoard, getOpen, onPoint, insertAll, down, softDown, up, liftGame, printContext) where
import Data.Tree
import Data.Tree.Zipper
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

type GameTree g = Tree g
type Context g = TreePos Full g
type InnerState g = Maybe (Context g, Int)
type GameState g r = State (InnerState g) r
type IOGameState g r = StateT (InnerState g) IO r


childNodes :: Context g -> [Context g]
childNodes context = helper (firstChild context)
	where
	helper :: Maybe (Context g) -> [Context g]
	helper Nothing = []
	helper (Just context) = context : (helper $ next context)

insertChild :: g -> Context g -> Context g
insertChild board = fromJust . parent . (insert $ singleton board) . children
	where
	fromJust (Just x) = x

printContext :: Show g => IOGameState g ()
printContext = lift . putStrLn . show =<< fmap (fmap (\(context,n) -> (flatten $ tree context, n))) get

initialState :: Int -> g -> InnerState g
initialState n board = Just (fromTree . singleton $ board, n-1)

singleton :: g -> GameTree g
singleton board = Node {rootLabel=board, subForest=[]}

--lookupState :: Eq g => g -> GameState g Bool
--lookupState board = flip fmap get $ (fmap $ (any ((==board).rootLabel)) . after . children) . fst

getBoard :: GameState g (Maybe g)
getBoard = fmap (fmap $ label . fst) get

getOpen :: GameState g Int
getOpen = fmap (maybe 0 snd) get

checkEmpty :: GameState g Bool
checkEmpty = flip fmap get $ maybe True (null . subForest . tree . fst) 

onPoint :: (Eq g) => g -> GameState g Bool
onPoint board =  fmap (maybe False $ (==board) . label . fst) get

getChildren :: GameState g [g]
getChildren = flip fmap get $ maybe [] ((map rootLabel) . subForest . tree . fst)

--getChildren' :: GameState g [GameState g g]
--getChildren' = flip fmap get $ maybe [] (\(context,n)-> fmap (flip (,) n) (helper context))
--	where
--	helper :: InnerState g -> [(InnerState g, g)]
--	helper Nothing = []
--	helper (Just (context,n)) = flip map (childNodes context) $ \context'-> (Just (context',n), label context')
getChildren' :: GameState g [(InnerState g,g)]
getChildren' = flip fmap get $ maybe [] (\(context,n) -> [(Just (context',n), label context')| context' <- childNodes context])

tryInsert :: g -> GameState g ()
tryInsert board = modify $ fmap (\(context,n) -> 
	if n==0 then (context,n) else (fromJust . parent . (insert $ singleton board) . children $ context, n-1))
	where
	fromJust (Just x) = x

insertAll :: [g] -> GameState g ()
insertAll xs = modify $ fmap (\(context,n) -> 
	if n<length xs then (context,n) else (helper context,n-(length xs)))
	where
	helper context = foldl (\context' -> \board -> (fromJust . parent . (insert $ singleton board) . children) context') context xs
	fromJust :: Maybe a -> a
	fromJust (Just x) = x

softDown :: Eq g => g -> GameState g ()
softDown board = modify $ (=<<) (\(context,n) -> fmap (flip (,) n) (helper context))
	where
	helper context = findBoard 0 (subForest . tree $ context) >>= (flip childAt context)
	findBoard _ [] = Nothing
	findBoard n (x:xs) = if rootLabel x == board then (Just n) else findBoard (n+1) xs


down :: Eq g => g -> GameState g ()
--down board = state $ \(context,n) -> ((),(fmap helper context, (n-) . length . flatten . toTree $ helper context))
down board = modify $ fmap (\(context,n) -> (helper context, newN n context))
	where
	helper context = case findBoard . subForest . toTree $ context of
		Just x -> fromTree x
		Nothing -> context
	newN n = (n+) . sum . (map length) . (map flatten) . (filter ((/=board) . rootLabel)) . subForest . tree
	findBoard [] = Nothing
	findBoard (x:xs) = if rootLabel x == board then Just x else findBoard xs

up :: InnerState g -> InnerState g
up = (=<<) (\(context,n) -> fmap (flip (,) n) $ parent context)
	where
	fromJust (Just x) = x

--tryInsert context board = 
--if (==0) . snd . rootLabel . toTree .root $ context then context else context

liftGame :: GameState g a -> StateT (Maybe (Context g, Int)) IO a
liftGame st = StateT { runStateT = return . (runState st) }

x1 = Node {rootLabel="1", subForest=[]}
x2 = Node {rootLabel="2", subForest=[]}
x3 = Node {rootLabel="3", subForest=[]}
x4 = Node {rootLabel="4", subForest=[]}
x5 = Node {rootLabel="5", subForest=[x1,x2]}
x6 = Node {rootLabel="6", subForest=[x3,x4]}
t = Node {rootLabel="7", subForest=[x5,x6]}



