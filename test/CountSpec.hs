module CountSpec where

import HW07
import Test.Hspec

-- Three divisions, with a division on each side.
threeDivs :: Term
threeDivs = Div (Div (Num 100) (Num 5)) (Div (Num 8) (Num 2))

countTerms :: [Term]
countTerms = [Num 7, tOk, tNested, threeDivs, Div (Num (-7)) (Num 2)]

-- Checks shared by both counting evaluators.
countEvaluatorSpec :: String -> (Term -> Counted Int) -> Spec
countEvaluatorSpec name eval =
  describe name $ do
    it "a number counts zero divisions: (7,0)" $
      runFromZero (eval (Num 7)) `shouldBe` (7, 0)
    it "a number leaves a nonzero starting counter unchanged" $
      runCounted (eval (Num 7)) 5 `shouldBe` (7, 5)
    it "one division adds one" $
      runFromZero (eval (Div (Num 10) (Num 2))) `shouldBe` (5, 1)
    it "tOk gives (43,2)" $
      runFromZero (eval tOk) `shouldBe` (43, 2)
    it "tNested gives (2,2)" $
      runFromZero (eval tNested) `shouldBe` (2, 2)
    it "threads the counter from the left child into the right child: (5,3)" $
      runFromZero (eval threeDivs) `shouldBe` (5, 3)
    it "starts from a nonzero counter: tOk from 10 gives (43,12)" $
      runCounted (eval tOk) 10 `shouldBe` (43, 12)

spec :: Spec
spec = do
  describe "Part 3" $
    countEvaluatorSpec "evalCount" evalCount

  describe "Part 7.1: countBind" $ do
    it "countBind tick (\\_ -> tick) counts twice" $
      runCounted (countBind tick (\_ -> tick)) 0 `shouldBe` ((), 2)
    it "countBind passes the first step's value to next" $
      runCounted (countBind (Counted (\c -> (c + 100, c))) (\x -> Counted (\c -> (x * 2, c)))) 3
        `shouldBe` (206 :: Int, 3)
    it "countBind gives next the counter produced by the first step" $
      runCounted (countBind tick (\_ -> Counted (\c -> (c * 10, c)))) 4 `shouldBe` (50 :: Int, 5)
    it "countBind runs next's computation and keeps its counter" $
      runCounted (countBind (Counted (\c -> (7, c))) (\x -> Counted (\c -> (x, c + 1)))) 0
        `shouldBe` (7 :: Int, 1)

  describe "Part 7.2: Counted instances" $ do
    it "pure 4 leaves the counter alone" $
      runCounted (pure 4 :: Counted Int) 3 `shouldBe` (4, 3)
    it "fmap changes the value but not the counter" $
      runCounted (fmap (* 2) (pure 21 :: Counted Int)) 6 `shouldBe` (42, 6)
    it "tick >>= (\\_ -> tick) counts twice" $
      runCounted (tick >>= \_ -> tick) 0 `shouldBe` ((), 2)
    it ">>= passes the value along: pure 5 >>= (\\x -> tick >>= \\_ -> pure x)" $
      runCounted (pure 5 >>= \x -> tick >>= \_ -> pure x) 1 `shouldBe` (5 :: Int, 2)
    it ">>= gives the second step the counter produced by the first step" $
      runCounted (tick >>= \_ -> Counted (\c -> (c * 10, c))) 4 `shouldBe` (50 :: Int, 5)

  describe "Part 7.4" $ do
    countEvaluatorSpec "evalCountM" evalCountM
    it "evalCountM agrees with evalCount from several starting counters" $
      [runCounted (evalCountM t) c | t <- countTerms, c <- [0, 1, 7]]
        `shouldBe` [runCounted (evalCount t) c | t <- countTerms, c <- [0, 1, 7]]
