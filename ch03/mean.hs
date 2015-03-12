mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (count xs)

mean' xs = foldl (+) 0 xs / fromIntegral (count xs)

mean'' :: Fractional a => [a] -> Maybe a
mean'' [] = Nothing
mean'' xs = Just (sum xs / (fromIntegral . count) xs)

mean''' l = (fst $ total_and_count l) / (snd $ total_and_count l)
  where total_and_count []     = (0, 0)
        total_and_count (x:xs) = ((fst $ total_and_count xs) + x,
                                  (snd $ total_and_count xs) + 1)
