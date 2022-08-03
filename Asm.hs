module Asm (sketch) where
import Control.Monad.State.Strict
import Debug.Trace
import Types
import Mangler

sketch :: UnrolledCode -> State Int Asm
sketch = (fmap concat) . (mapM convert)

convert :: UnrolledInstruction -> State Int Asm
convert (UnrolledReturn n) = return [("", PushVal n)]
convert (UnrolledCreateProc params body) = do
  after_lambda <- get_var
  entry_point <- get_var
  body' <- sketch body
  return ([("", Jmp after_lambda),
           (entry_point, Swym)]
          ++ body'
          ++ [("", Done),
              (after_lambda, PushLoc entry_point),
              ("", SetParams params),
              ("", CreateProc)])
convert (UnrolledApplyEnv var) = return [("", SetVar var),
                                         ("", ApplyEnv)]
convert (UnrolledDiff min sub) = do
  min' <- convert min
  sub' <- convert sub
  return (min'
          ++ [("", PopMoveVal1Val)]
          ++ sub'
          ++ [("", PopMoveVal2Val),
              ("", SubValVal1Val2)])
convert (UnrolledZeroPred n) = do
  n' <- convert n
  return (n'
          ++ [("", PushOneIfZeroValVal)])
convert (UnrolledExtendEnv var val) = do
             val' <- convert val
             return (val'
                     ++ [("", SetVar var),
                         ("", ExtendEnv),
                         ("", PopVal)])
convert (UnrolledExtendEnvRec var vals pbody) = do
                        after_proc <- get_var
                        entry_point <- get_var
                        pbody' <- sketch pbody
                        return ([("", Jmp after_proc),
                                 (entry_point, Swym)]
                                ++ pbody'
                                ++ [("", Done),
                                    (after_proc, PushLoc entry_point),
                                    ("", SetParams vals),
                                    ("", CreateRecProc var)])
convert (UnrolledIf pred cons alt) = do
  pred' <- convert pred
  after_cons <- get_var
  cons' <- sketch cons
  after_alt <- get_var
  alt' <- sketch alt
  return (pred'
          ++ [("", BzVal after_cons)]
          ++ cons'
          ++ [("", Jmp after_alt),
              (after_cons, PopVal)]
          ++ alt'
          ++ [(after_alt, Swym)])
convert (UnrolledApplication rator rands) = do
             rator' <- convert rator
             args <- fmap Apply (sequence (map convert rands))
             return (rator'
                     ++ [("", args)])
