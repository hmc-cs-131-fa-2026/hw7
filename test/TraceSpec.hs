module TraceSpec where

import HW07
import Test.Hspec

-- Left and right leaves differ, so swapping the left and right traces changes the text.
asymmetric :: Term
asymmetric = Div (Num 10) (Num 3)

asymmetricTrace :: String
asymmetricTrace =
  "eval(Num 10) = 10\n\
  \eval(Num 3) = 3\n\
  \eval(Div (Num 10) (Num 3)) = 3\n"

tOkTrace :: String
tOkTrace =
  "eval(Num 1999) = 1999\n\
  \eval(Num 2) = 2\n\
  \eval(Div (Num 1999) (Num 2)) = 999\n\
  \eval(Num 23) = 23\n\
  \eval(Div (Div (Num 1999) (Num 2)) (Num 23)) = 43\n"

traceTerms :: [Term]
traceTerms = [Num 7, asymmetric, tOk, tNested, Div (Num 5) (Div (Num 9) (Num 3))]

-- Checks shared by both tracing evaluators.
traceEvaluatorSpec :: String -> (Term -> Traced Int) -> Spec
traceEvaluatorSpec name eval =
  describe name $ do
    it "a number records one line" $
      eval (Num 7) `shouldBe` Traced "eval(Num 7) = 7\n" 7
    it "records left child, then right child, then the division itself" $
      eval asymmetric `shouldBe` Traced asymmetricTrace 3
    it "produces the exact trace for tOk" $
      eval tOk `shouldBe` Traced tOkTrace 43
    it "traceSummary of tOk is (43,5)" $
      traceSummary (eval tOk) `shouldBe` (43, 5)

spec :: Spec
spec = do
  describe "Part 4" $
    traceEvaluatorSpec "evalTrace" evalTrace

  describe "Part 8.1: Traced instances" $ do
    it "pure 3 has an empty log" $
      (pure 3 :: Traced Int) `shouldBe` Traced "" 3
    it "fmap changes the value but not the log" $
      fmap (+ 1) (Traced "log" 1) `shouldBe` (Traced "log" 2 :: Traced Int)
    it ">>= puts the first log before the second" $
      (Traced "first " 1 >>= \x -> Traced "second" (x + 1)) `shouldBe` (Traced "first second" 2 :: Traced Int)
    it "record adds a message with no useful value" $
      (record "hi" >>= \_ -> pure 5) `shouldBe` (Traced "hi" 5 :: Traced Int)
    it "a chain of records keeps them in order" $
      (record "a" >>= \_ -> record "b" >>= \_ -> record "c") `shouldBe` Traced "abc" ()

  describe "Part 8.3" $ do
    traceEvaluatorSpec "evalTraceM" evalTraceM
    it "evalTraceM agrees exactly with evalTrace on several terms" $
      map evalTraceM traceTerms `shouldBe` map evalTrace traceTerms
