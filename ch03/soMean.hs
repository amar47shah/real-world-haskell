import Control.Applicative
import Control.Arrow
import Data.Tuple

mean :: [Double] -> Double
mean l = sum l / (fromIntegral . length) l

--using Data.Tuple
soMean :: [Double] -> Double
soMean = uncurry (/) . total_and_count
  where total_and_count []     = (0, 0)
        total_and_count (x:xs) = (x + (fst . total_and_count) xs,
                                  1 + (snd . total_and_count) xs)

--using Control.Arrow and Data.Tuple
kindaMean :: [Double] -> Double
kindaMean = uncurry (/) . (sum &&& fromIntegral . length)

--using Control.Applicative
terriblyMean :: [Double] -> Double
terriblyMean = (/) <$> sum <*> fromIntegral . length
