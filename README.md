# CS 131 HW 7: Discovering Monads by Building Interpreters

The assignment itself (what to build, and why) is on the course site:
**https://www.cs.hmc.edu/~bang/cs131/assignments/hw07/**

This README only covers the mechanics.

## Files

- `HW07.hs`: the **only file you edit**. Work through the TODOs in order, and keep every evaluator.
  The manual versions are part of the point.
- `test/`: the Hspec test suite. Don't edit it.

## Working in GHCi

```
ghci HW07.hs
```

After each change, use `:reload`. When everything is done, `all snd checks` should print `True`.

## Running the tests

From the top of the repo, on the course server:

```
runhaskell -itest test/Spec.hs                     # the whole suite
runhaskell -itest test/Spec.hs --match "evalSafe"  # just the tests whose names mention evalSafe
```

Tests are grouped by the assignment's Parts, so `--match "Part 3"` runs only Part 3's tests. Until you finish a TODO,
its tests fail with `Prelude.undefined`, and the message points at the line in `HW07.hs`. That's expected. Each test
fails on its own, so the rest of the suite still runs.

The tests check *behavior*. They can't tell whether your monadic evaluators actually let the `Monad` instance do the
plumbing, or whether your manual ones still do it by hand. That part is up to you, and it's what your written report
is about.

## Submitting

```
runhaskell -itest test/Spec.hs > testing.log
```

Then, on Gradescope (**HW 07: Discovering Monads by Building Interpreters**), paste the contents of `testing.log`,
upload `HW07.hs`, and answer the report questions. Commit and push your work too. See the course site's
"The Homework Workflow" page for the full process.
