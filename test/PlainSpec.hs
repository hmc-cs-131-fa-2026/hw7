module PlainSpec where

import HW07
import Test.Hspec

-- A few terms used by several tests. Integer division follows Haskell's `div`,
-- which rounds toward negative infinity: (-7) `div` 2 == -4.
ordinaryTerms :: [Term]
ordinaryTerms =
  [ Num 7
  , Num (-3)
  , Div (Num 10) (Num 2)
  , Div (Num (-7)) (Num 2)
  , Div (Num 100) (Div (Num 10) (Num 2))
  , tOk
  , tNested
  ]

spec :: Spec
spec = do
  describe "Part 1: evalPlain" $ do
    it "evaluates a number to itself" $
      evalPlain (Num 7) `shouldBe` 7
    it "evaluates a negative number to itself" $
      evalPlain (Num (-3)) `shouldBe` (-3)
    it "evaluates a single division" $
      evalPlain (Div (Num 10) (Num 2)) `shouldBe` 5
    it "uses Haskell's div for negative numbers: (-7) `div` 2 == -4" $
      evalPlain (Div (Num (-7)) (Num 2)) `shouldBe` (-4)
    it "evaluates tOk to 43" $
      evalPlain tOk `shouldBe` 43
    it "evaluates tNested to 2" $
      evalPlain tNested `shouldBe` 2
    it "evaluates a division whose denominator is itself a division" $
      evalPlain (Div (Num 100) (Div (Num 10) (Num 2))) `shouldBe` 20

  describe "Part 6.4: evalPlainM" $ do
    it "evaluates tOk to 43" $
      unPlain (evalPlainM tOk) `shouldBe` 43
    it "agrees with evalPlain on several ordinary terms" $
      map (unPlain . evalPlainM) ordinaryTerms `shouldBe` map evalPlain ordinaryTerms
