module GenericSpec where

import HW07
import Test.Hspec

genericTerms :: [Term]
genericTerms =
  [ Num 7, tOk, tNested, Div (Num (-7)) (Num 2)
  , Div (Div (Num 100) (Num 5)) (Div (Num 8) (Num 2)) ]

spec :: Spec
spec =
  describe "Part 9: evalWith" $ do
    it "evalPlainW agrees with evalPlainM" $
      map (unPlain . evalPlainW) genericTerms `shouldBe` map (unPlain . evalPlainM) genericTerms
    it "evalSafeW agrees with evalSafeM, including on tBad" $
      map evalSafeW (tBad : Div tOk (Num 0) : genericTerms)
        `shouldBe` map evalSafeM (tBad : Div tOk (Num 0) : genericTerms)
    it "evalCountW agrees with evalCountM from several starting counters" $
      [runCounted (evalCountW t) c | t <- genericTerms, c <- [0, 1, 7]]
        `shouldBe` [runCounted (evalCountM t) c | t <- genericTerms, c <- [0, 1, 7]]
    it "evalTraceW agrees with evalTraceM, trace text and order included" $
      map evalTraceW genericTerms `shouldBe` map evalTraceM genericTerms
    it "evalWith calls afterNode on every node: tOk gives 5 trace lines" $
      traceSummary (evalTraceW tOk) `shouldBe` (43, 5)
