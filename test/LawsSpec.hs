module LawsSpec where

import HW07
import Test.Hspec

-- Concrete examples of the functor and monad laws (Parts 6.3 and 9.1).
-- Each law is checked for specific values, not proven in general.

safeValues :: [SafeResult Int]
safeValues = [Value 20, Value 0, Exception "oops"]

safeF :: Int -> SafeResult Int
safeF x = safeDiv 100 x          -- fails when x == 0

safeG :: Int -> SafeResult Int
safeG y = Value (y + 1)

tracedValues :: [Traced Int]
tracedValues = [Traced "m" 3, Traced "" 0]

tracedF :: Int -> Traced Int
tracedF x = Traced "f" (x + 1)

tracedG :: Int -> Traced Int
tracedG y = Traced "g" (y * 2)

-- Counted wraps a function, so we compare by running both sides from the same counters.
countedM :: Counted Int
countedM = tick >>= \_ -> pure 3

countedF :: Int -> Counted Int
countedF x = tick >>= \_ -> pure (x + 1)

countedG :: Int -> Counted Int
countedG y = Counted (\c -> (y * c, c + 2))

runAll :: Counted a -> [(a, Int)]
runAll m = [runCounted m c | c <- [0, 1, 7]]

spec :: Spec
spec = do
  describe "Functor laws" $ do
    it "SafeResult: fmap id == id" $
      map (fmap id) safeValues `shouldBe` safeValues
    it "SafeResult: fmap (g . f) == fmap g . fmap f" $
      map (fmap ((* 2) . (+ 1))) safeValues `shouldBe` map (fmap (* 2) . fmap (+ 1)) safeValues
    it "Traced: fmap id == id" $
      map (fmap id) tracedValues `shouldBe` tracedValues
    it "Traced: fmap (g . f) == fmap g . fmap f" $
      map (fmap ((* 2) . (+ 1))) tracedValues `shouldBe` map (fmap (* 2) . fmap (+ 1)) tracedValues
    it "Counted: fmap id == id (observed from several counters)" $
      runAll (fmap id countedM) `shouldBe` runAll countedM

  describe "Monad law: left identity (pure x >>= f == f x)" $ do
    it "SafeResult, with a step that succeeds and one that fails" $
      [pure x >>= safeF | x <- [5, 0]] `shouldBe` [safeF x | x <- [5, 0]]
    it "Traced" $
      (pure 4 >>= tracedF) `shouldBe` tracedF 4
    it "Counted (observed from several counters)" $
      runAll (pure 4 >>= countedF) `shouldBe` runAll (countedF 4)

  describe "Monad law: right identity (m >>= pure == m)" $ do
    it "SafeResult: Value 7 >>= pure == Value 7" $
      (Value 7 >>= pure) `shouldBe` (Value 7 :: SafeResult Int)
    it "SafeResult: Exception \"oops\" >>= pure == Exception \"oops\"" $
      (Exception "oops" >>= pure) `shouldBe` (Exception "oops" :: SafeResult Int)
    it "Traced" $
      map (>>= pure) tracedValues `shouldBe` tracedValues
    it "Counted (observed from several counters)" $
      runAll (countedM >>= pure) `shouldBe` runAll countedM

  describe "Monad law: associativity ((m >>= f) >>= g == m >>= (\\x -> f x >>= g))" $ do
    it "SafeResult" $
      map (\m -> (m >>= safeF) >>= safeG) safeValues
        `shouldBe` map (\m -> m >>= (\x -> safeF x >>= safeG)) safeValues
    it "Traced (logs come out in the same order either way)" $
      map (\m -> (m >>= tracedF) >>= tracedG) tracedValues
        `shouldBe` map (\m -> m >>= (\x -> tracedF x >>= tracedG)) tracedValues
    it "Counted (observed from several counters)" $
      runAll ((countedM >>= countedF) >>= countedG)
        `shouldBe` runAll (countedM >>= (\x -> countedF x >>= countedG))
