import qualified Data.Set as Set
import Data.List (minimumBy)
import System.Environment (getArgs)

main = do
    (filename:_) <- getArgs
    col <- readFile filename
    let (v, e) = readCol col
        (p, w, s) = minimalPartition (v, e) (-1)
    putStrLn $ "Found solution with weight " ++ show w ++ " in " ++ show s ++ " steps."
    print p

type Graph a = ([a], [Edge a])
type Edge a = (a, a)
type Partition a = (Set.Set a, Set.Set a)

-- По ТЗ
basicLocalSearch :: (Ord a) => Graph a -> Set.Set a
basicLocalSearch g = fst p
    where (p, _, _) = minimalPartition g (-1)

-- Наименьшее разбиение, полученное локальным поиском за steps шагов
minimalPartition :: (Ord a) => Graph a -> Int -> (Partition a, Int, Int)
minimalPartition ([], _) _ = error "empty graph"
minimalPartition (vs, es) steps = doLocalSearch partitionNeighbours weight steps initial
    where weight = partitionWeight es
          initial = (Set.fromList xs, Set.fromList ys)
              where (xs, ys) = splitAt (length vs `div` 2) vs

-- Обобщённый локальный поиск, минимизирующий fitness
doLocalSearch :: (Ord b) => (a -> [a]) -> (a -> b) -> Int -> a -> (a, b, Int)
doLocalSearch neighbours fitness maxSteps x = search (x, fitness x, 0)
    where search result@(x, f, s)
              | s == maxSteps       = result
              | null (neighbours x) = result
              | otherwise           = if f' < f then search (x', f', s+1)
                                                else result
              where x' = minimumBy compareFitness (neighbours x)
                    f' = fitness x'
          x1 `compareFitness` x2 = fitness x1 `compare` fitness x2

-- Вспомогательные функции для нашей задачи

swap :: (Ord a) => (a, a) -> Partition a ->  Partition a
swap (x, y) (xs, ys) = (replace x y xs, replace y x ys)
    where replace a b = Set.insert b . Set.delete a

partitionNeighbours :: (Ord a) => Partition a -> [Partition a]
partitionNeighbours p@(xs, ys) = map (`swap` p) swaps
    where swaps = [(x, y) | x <- Set.toList xs, y <- Set.toList ys]

partitionWeight :: (Ord a) => [Edge a] -> Partition a -> Int
partitionWeight edges p = length $ filter (badEdge p) edges
    where badEdge (x, _) (u, v) = Set.member u x `xor` Set.member v x

readCol :: String -> Graph Int
readCol = vsFromJust . foldl applyColLine (Nothing, []) . filter (not . null) . map words . lines
    where applyColLine g ("c":_) = g
          applyColLine (Nothing, []) ["p", "edge", v, _] = (Just [1..read v], [])
          applyColLine (Just _, _) ["p", _, _, _] = error "Duplicate problem line"
          applyColLine (Nothing, _) ["e", _, _] = error "Edge descriptor before problem line"
          applyColLine (Just vs, es) ["e", u, v] = (Just vs, (read u, read v):es)
          applyColLine _ ws = error $ "Unexpected line: " ++ show (unwords ws)
          vsFromJust (Nothing, es) = error "No problem line"
          vsFromJust (Just vs, es) = (vs, es)

xor :: Bool -> Bool -> Bool
True `xor` False = True
False `xor` True = True
_ `xor` _ = False
