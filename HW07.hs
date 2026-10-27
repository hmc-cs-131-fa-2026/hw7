{-|
CS 131 — Homework 07: Discovering Monads by Building Interpreters

Read the assignment page alongside this file:
  https://www.cs.hmc.edu/~bang/cs131/assignments/hw07/

Implement each TODO in order. An `undefined` marks work to complete.
The file is designed to load in GHCi before all TODOs are finished;
calling an unfinished function will fail until you implement it.

  :load HW07.hs
  :reload

Please keep all the distinct evaluator names. Do not delete the
manual versions after writing the monadic ones: comparing them is
one of the main points of the assignment.
-}

module HW07 where

-- ============================================================================
-- PART 0. The tiny language and sample terms (provided)
-- ============================================================================

data Term
  = Num Int
  | Div Term Term
  deriving (Show, Eq)

tOk :: Term
tOk = Div (Div (Num 1999) (Num 2)) (Num 23)

tBad :: Term
tBad = Div (Num 1) (Num 0)

-- A useful additional example with two divisions.
tNested :: Term
tNested = Div (Div (Num 8) (Num 2)) (Num 2)

-- ============================================================================
-- PART 1. Plain evaluation
-- ============================================================================

-- TODO 1: Fill in both cases. This evaluator may throw a Haskell exception
-- on division by zero; that is expected for this version.
evalPlain :: Term -> Int
evalPlain (Num n)   = undefined
evalPlain (Div t u) = undefined

-- Checks:
--   evalPlain (Num 7)  ==> 7
--   evalPlain tOk      ==> 43
--   evalPlain tNested  ==> 2

-- ============================================================================
-- PART 2. An evaluator that returns an error as a value
-- ============================================================================

data SafeResult a
  = Exception String
  | Value a
  deriving (Show, Eq)

-- TODO 2a: If the denominator is zero, return:
--   Exception "division by zero"
-- Otherwise return the quotient wrapped in Value.
safeDiv :: Int -> Int -> SafeResult Int
safeDiv a b = undefined

-- TODO 2b: Use nested `case` expressions to handle failures *explicitly*.
-- Do NOT use (>>=), `do`, or safeBind yet.
-- HINT: When the first child fails, return its error immediately.
evalSafe :: Term -> SafeResult Int
evalSafe (Num n) = undefined
evalSafe (Div t u) =
  case evalSafe t of
    Exception msg -> undefined          -- TODO: propagate error
    Value a ->
      case evalSafe u of
        Exception msg -> undefined      -- TODO: propagate error
        Value b       -> undefined      -- TODO: safely divide a by b

-- Checks:
--   safeDiv 10 2  ==> Value 5
--   safeDiv 10 0  ==> Exception "division by zero"
--   evalSafe tOk  ==> Value 43
--   evalSafe tBad ==> Exception "division by zero"

-- ============================================================================
-- PART 3. Count how many successful divisions occur
-- ============================================================================

-- Counted a is a computation that receives a counter, then returns
-- both its result (a) and a new counter.
--
-- Read: Counted a ~ Int -> (a, Int).
newtype Counted a = Counted (Int -> (a, Int))

-- PROVIDED: take the function back out of a Counted, and run it on a
-- starting counter.
runCounted :: Counted a -> Int -> (a, Int)
runCounted (Counted f) = f

-- Convenient way to run a counted computation starting with counter 0.
runFromZero :: Counted a -> (a, Int)
runFromZero computation = runCounted computation 0

-- TODO 3: Evaluate the children and thread the counter manually.
-- A Num does not increment the counter.
-- A successful Div increments it exactly once after evaluating both children.
-- Division by zero still throws an ordinary exception in this version.
evalCount :: Term -> Counted Int
evalCount (Num n) = Counted $ \count -> undefined

evalCount (Div t u) = Counted $ \count0 ->
  let (a, count1) = undefined   -- TODO: evaluate t starting at count0
      (b, count2) = undefined   -- TODO: evaluate u starting at count1
  in undefined                 -- TODO: quotient and updated counter

-- Checks:
--   runFromZero (evalCount (Num 7)) ==> (7,0)
--   runFromZero (evalCount tOk)     ==> (43,2)
--   runFromZero (evalCount tNested) ==> (2,2)

-- ============================================================================
-- PART 4. Return a trace as data
-- ============================================================================

-- A traced computation carries text and an ordinary result.
data Traced a = Traced String a
  deriving (Show, Eq)

traceLine :: Term -> Int -> String
traceLine term n =
  "eval(" ++ show term ++ ") = " ++ show n ++ "\n"

-- TODO 4: Return a line for every Num and every Div. Make each line with
-- traceLine, so the format matches the tests.
-- For division, combine logs in the order: left, right, current node.
-- Division by zero still throws an ordinary exception in this version.
evalTrace :: Term -> Traced Int
evalTrace (Num n) = undefined

evalTrace (Div t u) =
  let Traced leftText  a = undefined
      Traced rightText b = undefined
      result = a `div` b
  in undefined                       -- TODO: combined trace and value

-- Checks:
--   evalTrace (Num 7)  ==> Traced "eval(Num 7) = 7\n" 7
--   case evalTrace tOk of
--     Traced text result -> (result, length (lines text))
--     ==> (43,5)

-- ============================================================================
-- PART 5. Factor out error-handling code WITHOUT typeclasses yet
-- ============================================================================

-- TODO 5a: Wrap an ordinary value as a successful SafeResult.
safePure :: a -> SafeResult a
safePure x = undefined

-- TODO 5b: Either propagate an Exception or pass a Value to next.
safeBind :: SafeResult a
         -> (a -> SafeResult b)
         -> SafeResult b
safeBind (Exception msg) next = undefined
safeBind (Value x)       next = undefined

-- TODO 5c: Reimplement safe evaluation with safePure, safeBind, and safeDiv.
-- Do not use nested case expressions here.
evalSafeWithBind :: Term -> SafeResult Int
evalSafeWithBind (Num n) = undefined
evalSafeWithBind (Div t u) = undefined

-- Checks:
--   safePure 5 ==> Value 5
--   safeBind (Value 7) (\x -> Value (x+1)) ==> Value 8
--   safeBind (Exception "oops") (\x -> Value (x+1))
--     ==> Exception "oops"
--   evalSafeWithBind tOk  ==> Value 43
--   evalSafeWithBind tBad ==> Exception "division by zero"

-- ============================================================================
-- PART 6. Genuine Functor / Applicative / Monad instances for SafeResult
-- ============================================================================

-- TODO 6a: fmap must preserve errors and transform successful values.
instance Functor SafeResult where
  fmap f (Exception msg) = undefined
  fmap f (Value x)       = undefined

-- TODO 6b: pure acts like safePure. (<*>) applies a wrapped function
-- to a wrapped value; propagate failure or use fmap as appropriate.
instance Applicative SafeResult where
  pure x = undefined
  Exception msg <*> other = undefined
  Value f       <*> other = undefined

-- TODO 6c: >>= should do exactly what safeBind does.
instance Monad SafeResult where
  Exception msg >>= next = undefined
  Value x       >>= next = undefined

-- TODO 6d: Rewrite safe evaluation using pure and >>= or do.
-- No pattern matching on SafeResult in this evaluator!
evalSafeM :: Term -> SafeResult Int
evalSafeM (Num n) = undefined
evalSafeM (Div t u) = undefined

-- Suggested law checks after completing the instances:
--   (Value 7 >>= pure) == Value 7
--   (Exception "oops" >>= pure) == (Exception "oops" :: SafeResult Int)

-- ============================================================================
-- PART 6 continued. A plain computation can also be represented as a monad.
-- ============================================================================

-- PROVIDED: These instances are intentionally trivial. There is no error,
-- counter, or log to manage. Bind simply applies the next function.
newtype Plain a = Plain a
  deriving (Show, Eq)

unPlain :: Plain a -> a
unPlain (Plain x) = x

instance Functor Plain where
  fmap f (Plain x) = Plain (f x)

instance Applicative Plain where
  pure = Plain
  Plain f <*> Plain x = Plain (f x)

instance Monad Plain where
  Plain x >>= next = next x

-- TODO 6e: Reimplement the plain evaluator using `pure` and `do`.
-- The newtype wrapper does not change the ordinary division behavior.
evalPlainM :: Term -> Plain Int
evalPlainM (Num n) = undefined
evalPlainM (Div t u) = undefined

-- Check:
--   unPlain (evalPlainM tOk) ==> 43

-- ============================================================================
-- PART 7. Factor out the counter threading, then make Counted a Monad
-- ============================================================================

-- PROVIDED: a counted step that increments the counter.
tick :: Counted ()
tick = Counted $ \count -> ((), count + 1)

-- TODO 7a: Factor the counter threading out of evalCount, the way safeBind
-- factored the error checks out of evalSafe. Run `first` on the incoming
-- counter, pass its value to `next`, and run the computation that `next`
-- returns on the updated counter. (Lab 7's followedBy did this for a stack.)
countBind :: Counted a -> (a -> Counted b) -> Counted b
countBind first next = undefined

-- PROVIDED: how to map over a counted computation without changing its count.
instance Functor Counted where
  fmap f (Counted computation) = Counted $ \count0 ->
    let (x, count1) = computation count0
    in (f x, count1)

-- PROVIDED: <*> threads the counter through the first and second computations.
-- TODO 7b: implement only `pure` here.
instance Applicative Counted where
  pure x = undefined
  (Counted first) <*> (Counted second) = Counted $ \count0 ->
    let (f, count1) = first count0
        (x, count2) = second count1
    in (f x, count2)

-- TODO 7c: >>= should do exactly what countBind does.
-- (Writing `first >>= next = countBind first next` is a fine answer.)
instance Monad Counted where
  first >>= next = undefined

-- TODO 7d: Use do notation and tick, NOT explicit counters.
evalCountM :: Term -> Counted Int
evalCountM (Num n) = undefined
evalCountM (Div t u) = undefined

-- Checks:
--   runFromZero (evalCountM (Num 7)) ==> (7,0)
--   runFromZero (evalCountM tOk)     ==> (43,2)
--   runFromZero (evalCountM tNested) ==> (2,2)

-- ============================================================================
-- PART 8. Genuine instances for Traced, plus a monadic evaluator
-- ============================================================================

-- PROVIDED: mapping changes the value, not the log.
instance Functor Traced where
  fmap f (Traced message x) = Traced message (f x)

-- PROVIDED: <*> concatenates the logs in evaluation order.
-- TODO 8a: implement only `pure` here.
instance Applicative Traced where
  pure x = undefined
  Traced text1 f <*> Traced text2 x = Traced (text1 ++ text2) (f x)

-- TODO 8b: Call the continuation and concatenate its log after the first log.
instance Monad Traced where
  Traced text1 x >>= next = undefined

-- PROVIDED: add a line to the log without contributing a useful value.
record :: String -> Traced ()
record message = Traced message ()

-- TODO 8c: Use do notation and record, NOT explicit log concatenation.
-- Record one line for EVERY Num and Div as in evalTrace.
evalTraceM :: Term -> Traced Int
evalTraceM (Num n) = undefined
evalTraceM (Div t u) = undefined

-- Checks:
--   evalTraceM (Num 7) ==> Traced "eval(Num 7) = 7\n" 7
--   case evalTraceM tOk of
--     Traced text result -> (result, length (lines text))
--     ==> (43,5)

-- ============================================================================
-- PART 9. One evaluator for all four
-- ============================================================================

-- TODO 9a: Write the evaluator once, for any monad m. It takes two functions:
--   afterNode   called after EVERY node (numbers too) is evaluated, with that
--               node and the value it evaluated to: afterNode (Num n) n for a
--               number, afterNode (Div t u) v for a division.
--   divide      gets the two evaluated children and produces the quotient.
-- A Num node: call afterNode, then return its value. A Div node: evaluate
-- left, evaluate right, divide, call afterNode on the result, then return it.
evalWith :: Monad m
         => (Term -> Int -> m ())
         -> (Int -> Int -> m Int)
         -> Term -> m Int
evalWith afterNode divide (Num n)   = undefined
evalWith afterNode divide (Div t u) = undefined

-- TODO 9b: Get each monadic evaluator back as one call to evalWith.
-- Each should agree with the matching ...M evaluator.
evalPlainW :: Term -> Plain Int
evalPlainW = undefined

evalSafeW :: Term -> SafeResult Int
evalSafeW = undefined

evalCountW :: Term -> Counted Int
evalCountW = undefined

evalTraceW :: Term -> Traced Int
evalTraceW = undefined

-- ============================================================================
-- Quick checks (provided)
-- ============================================================================

-- These checks deliberately stay simple: a list of names and Boolean results.
-- Run them when you have finished the implementations:
--
--   checks
--   all snd checks
--
-- HINT: If a check produces an exception involving `undefined`, revisit
-- the corresponding TODO first.

traceSummary :: Traced a -> (a, Int)
traceSummary (Traced messages result) = (result, length (lines messages))

checks :: [(String, Bool)]
checks =
  [ ("plain",         evalPlain tOk == 43)
  , ("safe success",  evalSafe tOk == Value 43)
  , ("safe failure",  evalSafe tBad == Exception "division by zero")
  , ("count",         runFromZero (evalCount tOk) == (43, 2))
  , ("trace",         traceSummary (evalTrace tOk) == (43, 5))
  , ("named bind",    evalSafeWithBind tOk == Value 43)
  , ("monadic plain", unPlain (evalPlainM tOk) == 43)
  , ("monadic safe",  evalSafeM tBad == Exception "division by zero")
  , ("monadic count", runFromZero (evalCountM tOk) == (43, 2))
  , ("monadic trace", traceSummary (evalTraceM tOk) == (43, 5))
  , ("trace order",   evalTraceM tOk == evalTrace tOk)
  , ("evalWith",      evalTraceW tOk == evalTraceM tOk
                        && runFromZero (evalCountW tOk) == (43, 2)
                        && evalSafeW tBad == Exception "division by zero")
  , ("right identity (success)", (Value 7 >>= pure) == (Value 7 :: SafeResult Int))
  , ("right identity (failure)",
       ((Exception "oops" >>= pure) :: SafeResult Int) == Exception "oops")
  ]

-- OPTIONAL BONUS: Combine error reporting with a counter.
--
-- One possible representation:
--
-- newtype SafeCount a = SafeCount (Int -> (SafeResult a, Int))
--
-- runSafeCount :: SafeCount a -> Int -> (SafeResult a, Int)
-- runSafeCount (SafeCount f) = f
--
-- Can you build pure, bind, and an evaluator that reports both a
-- division-by-zero error AND the number of successful divisions before it?
-- See the writeup for the required behavior of bonusExample.

bonusExample :: Term
bonusExample = Div (Div (Num 12) (Num 3)) (Num 0)
