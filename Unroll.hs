module Unroll where
import Types

unroll_simple :: SimpleCps -> UnrolledInstruction
unroll_simple (SimpleConst n) = UnrolledReturn n
unroll_simple (SimpleLambda ps b) = UnrolledCreateProc ps (unroll b)
unroll_simple (SimpleVar v) = UnrolledApplyEnv v
unroll_simple (SimpleDiff min sub) = UnrolledDiff
                                     (unroll_simple min)
                                     (unroll_simple sub)
unroll_simple (SimpleZeroPred n) = UnrolledZeroPred (unroll_simple n)

unroll :: Cps -> UnrolledCode
unroll (SimpleCps s) = [unroll_simple s]
unroll (CpsLet var simple body) = UnrolledExtendEnv var (unroll_simple simple)
                                  : unroll body
unroll (CpsLetrec name params pbody body) = UnrolledExtendEnvRec name params (unroll pbody)
                                            : unroll body
unroll (CpsIf pred con alt) = UnrolledIf
                              (unroll_simple pred)
                              con'
                              alt'
                              : then'
  where (con', alt', then') = merge (unroll con) (unroll alt)
unroll (CpsCall rator rands) = [UnrolledApplication
                                (unroll_simple rator)
                                (map unroll_simple rands)]

merge :: UnrolledCode -> UnrolledCode -> (UnrolledCode, UnrolledCode, UnrolledCode)
merge branch1@(x : xs) branch2@(y : ys)
  | branch1 == branch2 = ([], [], branch1)
  | otherwise = sew (x, y) (merge xs ys)
merge [] [] = ([], [], [])

sew :: (UnrolledInstruction, UnrolledInstruction)
    -> (UnrolledCode, UnrolledCode, UnrolledCode)
    -> (UnrolledCode, UnrolledCode, UnrolledCode)
sew (x, y) (xs, ys, zs) = (x : xs, y : ys, zs)
