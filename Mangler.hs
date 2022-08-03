module Mangler where
import Numeric
import Control.Monad.State.Strict
import Debug.Trace
import Types

int_to_var :: Int -> String
int_to_var n = "v" ++ replicate (7 - length unpadded) '0' ++ unpadded
  where unpadded = showHex n ""

get_var :: State Int String
get_var = do
  var <- get
  put (var + 1)
  return (int_to_var var)

mangle_var :: String -> State (Int, [(String, String)]) String
mangle_var s = do
  (n, rs) <- get
  case lookup s rs of
    Just r -> return r
    Nothing -> (do
                   put (n + 1, (s, v) : rs)
                   return v)
               where v = int_to_var n

mangle :: Unoptimised -> State Int Unoptimised
mangle c = do
  sn <- get
  let (c', (n, _)) = runState (mangleM c) (sn, [])
  put n
  return c'

mangleM :: Unoptimised -> State (Int, [(String, String)]) Unoptimised
mangleM c@(UnoptConst n) = return c
mangleM (UnoptVar s) = fmap UnoptVar (mangle_var s)
mangleM (UnoptDiff m s) = do
  m' <- mangleM m
  s' <- mangleM s
  return (UnoptDiff m' s')
mangleM (UnoptZeroPred x) = fmap UnoptZeroPred (mangleM x)
mangleM (UnoptLambda ps b) = do
  ps' <- sequence (map mangle_var ps)
  b' <- mangleM b
  return (UnoptLambda ps' b')
mangleM (UnoptIf p c a) = do
  p' <- mangleM p
  c' <- mangleM c
  a' <- mangleM a
  return (UnoptIf p' c' a')
mangleM (UnoptLet v e b) = do
  v' <- mangle_var v
  e' <- mangleM e
  b' <- mangleM b
  return (UnoptLet v' e' b')
mangleM (UnoptLetrec n ps pb b) = do
  n' <- mangle_var n
  ps' <- sequence (map mangle_var ps)
  pb' <- mangleM pb
  b' <- mangleM b
  return (UnoptLetrec n' ps' pb' b')
mangleM (UnoptCall rand rators) = do
  rand' <- mangleM rand
  rators' <- sequence (map mangleM rators)
  return (UnoptCall rand' rators')
