import Data.Char (isDigit)
import Data.List (delete, minimumBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (deepseq)
import System.Directory (setCurrentDirectory, getDirectoryContents)
import System.IO (stdout, hFlush)

main = do
    (dir:_) <- getArgs
    setCurrentDirectory dir
    names' <- getDirectoryContents "."
    let names = filter ((/= '.') . head) names'
    mapM_ runInstance names

runInstance :: String -> IO ()
runInstance name = do
    putStr $ "Solving instance " ++ name ++ "..."
    hFlush stdout
    putStr " done "
    benchmark solveTSPNearestNeighbour name
    putStr " using NN"
    hFlush stdout
    putStr " and "
    benchmark solveTSPNearestInsertion name
    putStrLn " using NI"

benchmark :: ([Point Double] -> Path Double) -> String -> IO ()
benchmark solver name = do
    text <- readFile name
    let ps = readTSPInstance text
    before <- ps `deepseq` getCPUTime
    let tour = solver ps
    after <- tour `deepseq` getCPUTime
    let time = fromInteger (after - before) / 10^12 -- picoseconds
    putStr $ "in " ++ show time ++ " seconds with tour length " ++ show (round $ pathLength tour)

type Point a = (a, a)

type Edge a = (Point a, Point a)
type Path a = [Edge a]

readTSPInstance :: String -> [Point Double]
readTSPInstance = foldl appendPoint [] . lines
    where appendPoint ps "" = ps
          appendPoint ps line@(c:_) = if isDigit c then readLine line : ps
                                                   else ps
          readLine line = (read x, read y) where [_, x, y] = words line

euclidianDistance :: (RealFloat a) => Point a -> Point a -> a
euclidianDistance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

edgeLength :: (RealFloat a) => Edge a -> a
edgeLength = uncurry euclidianDistance

pathLength :: (RealFloat a) => Path a -> a
pathLength = sum . map edgeLength

solveTSPNearestNeighbour :: (RealFloat a) => [Point a] -> Path a
solveTSPNearestNeighbour [] = []
solveTSPNearestNeighbour [p] = [(p, p)]
solveTSPNearestNeighbour (start:ps) = (start, end) : path
    where path@((end, _):_) = solve start ps []
          solve _ [] es = es
          solve p left es = solve p' (delete p' left) ((p', p):es)
               where p' = nearestNeighbour p left

nearestNeighbour :: (RealFloat a) => Point a -> [Point a] -> Point a
nearestNeighbour p = minimumBy $ comparing (euclidianDistance p)

solveTSPNearestInsertion :: (RealFloat a) => [Point a] -> Path a
solveTSPNearestInsertion [] = []
solveTSPNearestInsertion [p] = [(p, p)]
solveTSPNearestInsertion ps = solve [(p1, p2), (p2, p1)] $ delete p1 $ delete p2 ps
    where (p1, p2) = minimumBy (comparing edgeLength) [(x, y) | x <- ps, y <- ps, x /= y]
          solve tour [] = tour
          solve tour ps = solve (insertPoint p e tour) (delete p ps)
              where p = nearestPoint (map fst tour) ps
                    e = nearestEdge p tour

nearestPoint :: (RealFloat a) => [Point a] -> [Point a] -> Point a
nearestPoint ps others = fst $ minimumBy (comparing snd) others'
    where others' = map (\p -> (p, distance p)) others
          distance p = minimum $ map (euclidianDistance p) ps

nearestEdge :: (RealFloat a) => Point a -> Path a -> Edge a
nearestEdge p = minimumBy (comparing $ cost p)
    where cost p e@(p1, p2) = edgeLength (p1, p) + edgeLength (p, p2) - edgeLength e

nearestInsertion :: (RealFloat a) => Path a -> [Point a] -> (Point a, Edge a)
nearestInsertion path ps = minimumBy (comparing cost) [(p, e) | p <- ps, e <- path]
    where cost (p, e@(p1, p2)) = edgeLength (p1, p) + edgeLength (p, p2) - edgeLength e

insertPoint :: (RealFloat a) => Point a -> Edge a -> Path a -> Path a
insertPoint p e@(p1, p2) = replaceFirst e [(p1, p), (p, p2)]

replaceFirst :: (Eq a) => a -> [a] -> [a] -> [a]
replaceFirst x xs = replacer
    where replacer [] = error "element not found"
          replacer (y:ys) = if x == y then xs ++ ys else y:replacer ys
