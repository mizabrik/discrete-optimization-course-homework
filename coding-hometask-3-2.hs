import Data.Char (isDigit)
import Data.List (delete, minimumBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (deepseq)
import System.Directory (setCurrentDirectory, getDirectoryContents)
import System.IO (stdout, hFlush)
import qualified Data.Map as M

main = do
    (dir:_) <- getArgs
    setCurrentDirectory dir
    names' <- getDirectoryContents "."
    let names = filter ((/= '.') . head) names'
    mapM_ runInstance names

runInstance :: String -> IO ()
runInstance name = do
    putStr $ "Instance " ++ name ++ "..."
    hFlush stdout
    putStr " done "
    benchmark lowerBoundTsp name
    putStrLn ""

benchmark :: ([Point Double] -> Double) -> String -> IO ()
benchmark solver name = do
    text <- readFile name
    let ps = readTSPInstance text
    before <- ps `deepseq` getCPUTime
    let bound = solver ps
    after <- bound `deepseq` getCPUTime
    let time = fromInteger (after - before) / 10^12 -- picoseconds
    putStr $ "in " ++ show time ++ " seconds with lower bound " ++ show (round bound)

type Point a = (a, a)

type Edge a = (Point a, Point a)
type Path a = [Edge a]
type Weighter a = Point a -> Point a -> a

mst :: (Real a) => [Point a] -> Weighter a -> [Edge a]
mst (v:vs) w = prim [(w v u, v, u) | u <- vs] []
    where prim [] es = es
          prim csts es = prim csts' ((v, u):es)
              where (_, v, u) = minimum csts
                    csts' = [min (w u u', u, u') c' | c'@(_, _, u') <- csts, u' /= u]

lowerBoundTsp :: (RealFloat a) => [Point a] -> a
lowerBoundTsp = tspHeldKarpBound 50

tspHeldKarpBound :: (RealFloat a) => Int -> [Point a] -> a
tspHeldKarpBound maxStep (v:vs) = best
    where (_, _, _, best) = until lastStep (doHeldKarp vs) (0, zeroMap vs, 2, 0)
          lastStep (step, _, _, _) = step >= maxStep

type Potential a = M.Map (Point a) a
type HeldKarpS a = (Int, Potential a, a, a)
doHeldKarp :: (RealFloat a) => [Point a] -> HeldKarpS a -> HeldKarpS a
doHeldKarp vs (i, pot, alpha, best) = (i + 1, pot', alpha', max best new)
    where tree = mst vs w
          degs = degrees vs tree
          w u v = euclidianDistance u v - pot M.! u - pot M.! v
          newPotential v = (+alpha * (2 - fromIntegral (degs M.! v)))
          pot' = M.mapWithKey newPotential pot
          alpha'  = alpha / 1.05
          new = sum $ map (uncurry euclidianDistance) tree

degrees :: (Real a) => [Point a] -> [Edge a] -> M.Map (Point a) Int
degrees vs es = foldl (flip ($)) (zeroMap vs) (map edgeUpdater es)
    where edgeUpdater (u, v) = inc u . inc v
          inc = M.update $ Just . (+1)

readTSPInstance :: String -> [Point Double]
readTSPInstance = foldl appendPoint [] . lines
    where appendPoint ps "" = ps
          appendPoint ps line@(c:_) = if isDigit c then readLine line : ps
                                                   else ps
          readLine line = (read x, read y) where [_, x, y] = words line

zeroMap :: (Ord k, Num a) => [k] -> M.Map k a
zeroMap ks = M.fromList $ zip ks $ repeat 0

euclidianDistance :: (RealFloat a) => Point a -> Point a -> a
euclidianDistance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2
