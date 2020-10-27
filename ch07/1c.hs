hunsD :: Integral a => a -> a
hunsD x = d
  where (except, _) = x `divMod` 100
        (_, d) = except `divMod` 10
