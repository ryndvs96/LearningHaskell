import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms n gen
    | n == 0    = ([], gen)
    | otherwise = (value : restOfList, finalGen)
    where (value, newGen) = random gen
          (restOfList, finalGen) = finiteRandoms (n - 1) newGen