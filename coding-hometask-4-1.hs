import Data.List (delete, intercalate)

-- Внешняя зависимость: limp-cbc.
-- Можно установить через cabal, для билда нужно установить c2hs
import Numeric.Limp.Program
import Numeric.Limp.Rep.Rep (Assignment, zOf)
import Numeric.Limp.Rep.IntDouble
import Numeric.Limp.Solvers.Cbc (solve)

main = do
    let (w, tour) = solveTSP 14 distances
    putStrLn $ "Found tour of length " ++ show w ++ ":"
    putStrLn $ showTour tour

distances :: [[Double]]
distances = [[0, 29, 82, 46, 68, 52, 72, 42, 51, 55, 29, 74, 23, 72, 46],
             [29, 0, 55, 46, 42, 43, 43, 23, 23, 31, 41, 51, 11, 52, 21],
             [82, 55, 0, 68, 46, 55, 23, 43, 41, 29, 79, 21, 64, 31, 51],
             [46, 46, 68, 0, 82, 15, 72, 31, 62, 42, 21, 51, 51, 43, 64],
             [68, 42, 46, 82, 0, 74, 23, 52, 21, 46, 82, 58, 46, 65, 23],
             [52, 43, 55, 15, 74, 0, 61, 23, 55, 31, 33, 37, 51, 29, 59],
             [72, 43, 23, 72, 23, 61, 0, 42, 23, 31, 77, 37, 51, 46, 33],
             [42, 23, 43, 31, 52, 23, 42, 0, 33, 15, 37, 33, 33, 31, 37],
             [51, 23, 41, 62, 21, 55, 23, 33, 0, 29, 62, 46, 29, 51, 11],
             [55, 31, 29, 42, 46, 31, 31, 15, 29, 0, 51, 21, 41, 23, 37],
             [29, 41, 79, 21, 82, 33, 77, 37, 62, 51, 0, 65, 42, 59, 61],
             [74, 51, 21, 51, 58, 37, 37, 33, 46, 21, 65, 0, 61, 11, 55],
             [23, 11, 64, 51, 46, 51, 51, 33, 29, 41, 42, 61, 0, 62, 23],
             [72, 52, 31, 43, 65, 29, 46, 31, 51, 23, 59, 11, 62, 0, 59],
             [46, 21, 51, 64, 23, 59, 33, 37, 11, 37, 61, 55, 23, 59, 0]]

type TSPSolution = Assignment (Int, Int) Int IntDouble

-- Ищет лучший гамильтонов цикл в графе с n+1 вершиной
-- и весами distances, всё как на лекции :)
solveTSP :: Int -> [[Double]] -> (Double, [Int])
solveTSP n distances = case solve prog of
               Left err -> error (show err)
               Right sol -> (unwrapR $ weight sol, getTour n sol)
    where prog = tspProgram n distances
          weight = (`eval` _objective prog)

showTour :: (Show a) => [a] -> String
showTour = intercalate " ⟶ " . map show

getTour :: Int -> TSPSolution -> [Int]
getTour n ass = 0 : getTour' [0..n] 0
    where getTour' left i = if next == 0 
                                then [0]
                                else next : getTour' (delete next left) next
              where [next] = filter (chosen i) left
          chosen i j = zOf ass (i, j) == Z 1

tspProgram :: Int -> [[Double]] -> Program (Int, Int) Int IntDouble
tspProgram n distances = minimise target constraint bounds
    where target = LR weights (R 0)
          constraint = foldl1 (:&&) $ neighbourConstraints ++ mtzConstraints
          bounds = map binary edges
          weights = [(Left (i, j), R c) | (i, cs) <- enumerate distances,
                                          (j, c) <- enumerate cs, i /= j]
          neighbourConstraints = do
              i <- vertices
              [inSum i :== con 1, outSum i :== c1]
          mtzConstraints = [r1 i .-. r1 j .+. z (i, j) (Z n) :<= con (Z (n-1))
                                | i <- [1..n], j <- [1..n], i /= j] 
          vertices = [0..n]
          neighbours i = filter (/= i) vertices
          edges = [(i, j) | i <- vertices, j <- neighbours i]
          inSum i = LZ [((j, i), Z 1) | j <- neighbours i] (Z 0)
          outSum i = LZ [((i, j), Z 1) | j <- neighbours i] (Z 0)

enumerate :: (Num b, Enum b) => [a] -> [(b, a)]
enumerate = zip [0..]
