import Data.List (find, delete, sortBy)
import Data.Maybe (fromJust)

-- Решение descision-задачи для произвольных весов
canPack :: (Num a, Ord a) => [a] -> [a] -> Bool
canPack [] _ = True
canPack _ [] = False
canPack (w:ws) (b:bs) = canPackIn [] b bs
    where canPackIn skip b bs = b >= w && canPack ws (b-w:bs ++ skip)
                                || not (null bs) && canPackIn (b:skip) (head bs) (tail bs)

-- Можно ли уместить weights в nBins корзин вместимости 1
solveBPDescision :: [Rational] -> Int -> Bool
solveBPDescision weights nBins = canPack (sortBy (flip compare) weights) (replicate nBins 1)

-- Минимальное целое n из [l, r] такой, что выполняется pred x
minMatching :: Integral a => (a -> Bool) -> a -> a -> Maybe a
minMatching pred l r
    | l > r     = Nothing
    | l == r    = if pred l then Just l else Nothing
    | l+1 == r  = if pred l then Just l else minMatching pred r r
    | otherwise = if pred mid then minMatching pred l mid
                              else minMatching pred mid r
                  where mid = (l + r) `div` 2

-- Минимальное число корзин вместимости 1, в которые можно уместить weights
solveBPEvaluation :: [Rational] -> Maybe Int
solveBPEvaluation weights = minMatching (solveBPDescision weights) 0 (length weights)

-- Разбиение weights на одну корзину и оставшиеся элементы
-- при некоторой оптимальной упаковке в корзины вместимости 1
extractBin :: [Rational] -> Maybe ([Rational], [Rational])
extractBin [] = Just ([], [])
extractBin (w:ws) = do
    nBins <- solveBPEvaluation (w:ws)
    let foundBin Nothing = True
        foundBin (Just (_, left)) = case solveBPEvaluation left of
            Nothing -> False
            Just nBins' -> nBins' < nBins
        increaseBin (Just (bin, left)) = do
            let canAdd w = solveBPEvaluation (w + sum bin : delete w left) == Just nBins
            new <- find canAdd left
            return (new : bin, delete new left)
        in until foundBin increaseBin (Just ([w], ws))

-- Оптимальное разбиение weights на корзины вместимости 1
solveBPSearch :: [Rational] -> Maybe [[Rational]]
solveBPSearch [] = Just []
solveBPSearch weights = do
    (bin, left) <- extractBin weights
    bins <- solveBPSearch left
    return (bin:bins)
