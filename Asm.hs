module Asm (sketch) where
import System.IO.Unsafe
import Types
import Unsafe

sketch :: UnrolledCode -> Asm
sketch = concatMap convert

convert :: UnrolledInstruction -> Asm
{-# NOINLINE convert #-}
convert (UnrolledReturn n) = [("", Push Val (LiteralInt n))]
convert (UnrolledCreateProc params body) = [("", Jmp after_lambda),
                                            (entry_point, Swym)]
                                           ++ sketch body
                                           ++ [("", GoBack),
                                               (after_lambda, Set Loc (LiteralLabel entry_point)),
                                               ("", Set Params (LiteralStringList params)),
                                               ("", Do CreateProc)]
                                           where after_lambda = unsafePerformIO get_identifier
                                                 entry_point = after_lambda ++ "'"
convert (UnrolledApplyEnv var) = [("", Set Var (LiteralString var)),
                                  ("", Do ApplyEnv)]
convert (UnrolledDiff min sub) = convert min
                                 ++ [("", PopMove Val1 Val)]
                                 ++ convert sub
                                 ++ [("", PopMove Val2 Val),
                                     ("", Sub Val Val1 Val2),
                                     ("", Pop Val1),
                                     ("", Pop Val2)]
convert (UnrolledZeroPred n) = convert n
                               ++ [("", PopMove Val1 Val),
                                   ("", PushOneIfZero Val Val1),
                                   ("", Pop Val1)]
convert (UnrolledExtendEnv var val) = convert val
                                      ++ [("", Set Var (LiteralString var)),
                                          ("", Do ExtendEnv),
                                          ("", Pop Val)]
convert (UnrolledExtendEnvRec var vals pbody) = [("", Jmp after_proc),
                                                 (entry_point, Swym)]
                                                ++ sketch pbody
                                                ++ [("", GoBack),
                                                    (after_proc,
                                                     Set Loc (LiteralLabel entry_point)),
                                                    ("", Set Var (LiteralString var)),
                                                    ("", Set Params (LiteralStringList vals)),
                                                    ("", Do ExtendEnvRec)]
                                                where after_proc = unsafePerformIO get_identifier
                                                      entry_point = after_proc ++ "'"
convert (UnrolledIf pred cons alt) = convert pred
                                     ++ [("", Bz Val after_cons),
                                         ("", Pop Val)]
                                     ++ sketch cons
                                     ++ [("", Jmp after_alt),
                                         (after_cons, Pop Val)]
                                     ++ sketch alt
                                     ++ [(after_alt, Swym)]
                                     where after_cons = unsafePerformIO get_identifier
                                           after_alt = after_cons ++ "'"
convert (UnrolledApplication rator rands) = convert rator
                                            ++ [("", Push Val LiteralBottom)]
                                            ++ sketch rands
                                            ++ [("", Do Apply)]
