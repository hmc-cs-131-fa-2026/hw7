module SafeSpec where

import HW07
import Test.Hspec

divByZero :: SafeResult Int
divByZero = Exception "division by zero"

-- The left child fails, so the right child must never be looked at.
-- If an evaluator touches it, this test crashes with the message below.
leftFailsFirst :: Term
leftFailsFirst = Div tBad (error "the right child was evaluated after the left child failed")

safeTerms :: [Term]
safeTerms =
  [ Num 7
  , tOk
  , tNested
  , tBad
  , Div (Num (-7)) (Num 2)
  , Div (Div (Num 1) (Num 0)) (Num 5)   -- failure in the left subtree
  , Div (Num 5) (Div (Num 1) (Num 0))   -- failure in the right subtree
  , Div (Num 5) (Div (Num 1) (Num 2))   -- denominator evaluates to 0
  ]

-- Checks shared by every safe evaluator.
safeEvaluatorSpec :: String -> (Term -> SafeResult Int) -> Spec
safeEvaluatorSpec name eval =
  describe name $ do
    it "evaluates a number to Value n" $
      eval (Num 7) `shouldBe` Value 7
    it "evaluates tOk to Value 43" $
      eval tOk `shouldBe` Value 43
    it "evaluates tNested to Value 2" $
      eval tNested `shouldBe` Value 2
    it "reports tBad as Exception \"division by zero\"" $
      eval tBad `shouldBe` divByZero
    it "propagates a failure from the left subtree" $
      eval (Div (Div (Num 1) (Num 0)) (Num 5)) `shouldBe` divByZero
    it "propagates a failure from the right subtree" $
      eval (Div (Num 5) (Div (Num 1) (Num 0))) `shouldBe` divByZero
    it "fails when the denominator evaluates to 0 (1 `div` 2 == 0)" $
      eval (Div (Num 5) (Div (Num 1) (Num 2))) `shouldBe` divByZero
    it "stops after a failure: never evaluates the right child" $
      eval leftFailsFirst `shouldBe` divByZero

spec :: Spec
spec = do
  describe "Part 2.1: safeDiv" $ do
    it "safeDiv 10 2 == Value 5" $
      safeDiv 10 2 `shouldBe` Value 5
    it "safeDiv 10 0 == Exception \"division by zero\"" $
      safeDiv 10 0 `shouldBe` divByZero
    it "safeDiv 0 5 == Value 0 (only the denominator matters)" $
      safeDiv 0 5 `shouldBe` Value 0
    it "uses div: safeDiv (-7) 2 == Value (-4)" $
      safeDiv (-7) 2 `shouldBe` Value (-4)

  describe "Part 2.2" $
    safeEvaluatorSpec "evalSafe" evalSafe

  describe "Part 5.1-5.2: safePure and safeBind" $ do
    it "safePure 5 == Value 5" $
      safePure (5 :: Int) `shouldBe` Value 5
    it "safeBind passes a Value to the next step" $
      safeBind (Value 7) (\x -> Value (x + 1)) `shouldBe` (Value 8 :: SafeResult Int)
    it "safeBind lets the next step fail" $
      safeBind (Value 7) (\x -> safeDiv x 0) `shouldBe` divByZero
    it "safeBind passes an Exception along unchanged" $
      safeBind (Exception "oops") (\x -> Value (x + 1)) `shouldBe` (Exception "oops" :: SafeResult Int)
    it "safeBind never calls the next step after an Exception" $
      safeBind (Exception "oops") (\_ -> error "next step was called") `shouldBe` (Exception "oops" :: SafeResult Int)

  describe "Part 5.3" $
    safeEvaluatorSpec "evalSafeWithBind" evalSafeWithBind

  describe "Part 6.1: SafeResult instances" $ do
    it "fmap transforms a Value" $
      fmap (+ 1) (Value 1) `shouldBe` (Value 2 :: SafeResult Int)
    it "fmap leaves an Exception unchanged" $
      fmap (+ 1) (Exception "e") `shouldBe` (Exception "e" :: SafeResult Int)
    it "fmap can change the type: fmap show (Value 3) == Value \"3\"" $
      fmap show (Value (3 :: Int)) `shouldBe` Value "3"
    it "pure 3 == Value 3" $
      (pure 3 :: SafeResult Int) `shouldBe` Value 3
    it "Value (+1) <*> Value 2 == Value 3" $
      (Value (+ 1) <*> Value 2) `shouldBe` (Value 3 :: SafeResult Int)
    it "Exception <*> anything keeps the Exception" $
      (Exception "f" <*> Value 2) `shouldBe` (Exception "f" :: SafeResult Int)
    it "Value f <*> Exception keeps the Exception" $
      (Value (+ 1) <*> Exception "x") `shouldBe` (Exception "x" :: SafeResult Int)
    it "Value 7 >>= (\\x -> Value (x + 1)) == Value 8" $
      (Value 7 >>= \x -> Value (x + 1)) `shouldBe` (Value 8 :: SafeResult Int)
    it "Exception \"oops\" >>= ... == Exception \"oops\", without calling the next step" $
      (Exception "oops" >>= \_ -> error "next step was called") `shouldBe` (Exception "oops" :: SafeResult Int)
    it ">>= lets the next step fail" $
      (Value 7 >>= \x -> safeDiv x 0) `shouldBe` divByZero

  describe "Part 6.2" $ do
    safeEvaluatorSpec "evalSafeM" evalSafeM
    it "evalSafeM agrees with evalSafe on every test term" $
      map evalSafeM safeTerms `shouldBe` map evalSafe safeTerms
