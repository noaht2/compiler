module Unroll where
import Types

unroll_simple :: SimpleCps -> UnrolledInstruction
unroll_simple (SimpleConst n) = UnrolledReturn n
unroll_simple (SimpleLambda ps b) = UnrolledCreateProc ps b
unroll_simple (SimpleVar v) = UnrolledApplyEnv v
unroll_simple (SimpleDiff min sub) = UnrolledDiff
                                     (unroll_simple min)
                                     (unroll_simple sub)
unroll_simple (SimpleZeroPred n) = UnrolledZeroPred (unroll_simple n)

unroll :: Cps -> UnrolledCode
unroll (SimpleCps s) = [unroll_simple s]
unroll (CpsLet var simple body) = UnrolledExtendEnv var (unroll_simple simple)
                                  : unroll body
unroll (CpsLetrec name params pbody body) = UnrolledExtendEnvRec name params pbody
                                            : unroll body
unroll (CpsIf pred con alt) = [UnrolledIf
                               (unroll_simple pred)
                               (unroll con)
                               (unroll alt)]
unroll (CpsCall rator rands) = [UnrolledApplication
                                (unroll_simple rator)
                                (map unroll_simple rands)]
