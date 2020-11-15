import Test.Hspec

rMul :: Integral a => a -> a -> a
rMul x y = go (abs x) (abs y) 0 (f x y)
  where go x' y' result sign
          | y' == 0 || x' == 0 = sign * result
          | otherwise = go x' (y' - 1) (result + x') sign
        f x' y'
          | (x' < 0 && y' < 0) || (x' > 0 && y' > 0) = 1
          | otherwise = -1
       
main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "0 x 0 is 0" $ do
      rMul 0 0 `shouldBe` 0
    it "0 x 5 is 0" $ do
      rMul 0 5 `shouldBe` 0
    it "4 x 0 is 0" $ do
      rMul 4 0 `shouldBe` 0
    it "5 x 6 is 30" $ do
      rMul 5 6 `shouldBe` 30
    it "-4 x -6 is 24" $ do
      rMul (-4) (-6) `shouldBe` 24
    it "-5 x 7 is -35" $ do
      rMul (-5) 7 `shouldBe` (-35)
    it "8 x -9 is -72" $ do
      rMul 8 (-9) `shouldBe` (-72)
