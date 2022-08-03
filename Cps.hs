module Cps (convert) where
import Data.List
import Types
import Control.Monad.State.Strict
import Mangler

send_to_cont :: SimpleCps -> SimpleCps -> Cps
send_to_cont exp cont = CpsCall cont [exp]

convert :: Unoptimised -> State Int Cps
convert prog = cps_of_exps [prog] (SimpleCps . (!! 0))

replace :: Eq a => [a] -> Int -> a -> [a]
replace xs i y = acc xs i y
  where acc (x : xs) 0 y = y : xs
        acc (x : xs) j y = x : acc xs (j - 1) y

cps_of_simple_exp :: Unoptimised -> State Int SimpleCps
cps_of_simple_exp (UnoptConst n) = return (SimpleConst n)
cps_of_simple_exp (UnoptVar v) = return (SimpleVar v)
cps_of_simple_exp (UnoptDiff m s) = do
  m' <- cps_of_simple_exp m
  s' <- cps_of_simple_exp s
  return (SimpleDiff m' s')
cps_of_simple_exp (UnoptZeroPred e) = fmap SimpleZeroPred (cps_of_simple_exp e)
cps_of_simple_exp (UnoptLambda ps b) = do
  cont_var <- get_var
  body <- (cps_of_exp (SimpleVar cont_var) b)
  return (SimpleLambda
          (ps ++ [cont_var])
          body)

cps_of_exps :: [Unoptimised] -> ([SimpleCps] -> Cps) -> State Int Cps
cps_of_exps exps builder = cps_of_rest exps
  where
    cps_of_rest exps
      = let pos = findIndex (not . simple) exps in
          case pos of
            Nothing -> fmap builder (mapM cps_of_simple_exp exps)
            Just pos' -> do
              var <- get_var
              body <- (cps_of_rest
                       (replace exps pos' (UnoptVar var)))
              cps_of_exp
                (SimpleLambda [var] body)
                (exps !! pos')

cps_of_exp :: SimpleCps -> Unoptimised -> State Int Cps
cps_of_exp cont (UnoptConst n) = return $ send_to_cont (SimpleConst n) cont
cps_of_exp cont (UnoptVar var) = return $ send_to_cont (SimpleVar var) cont
cps_of_exp cont (UnoptLambda params body) = do
  cont_var <- get_var
  body <- (cps_of_exp
           (SimpleVar cont_var)
           body)
  return $ send_to_cont
    (SimpleLambda
     (params ++ [cont_var])
     body)
                                             
                                            cont
cps_of_exp cont (UnoptZeroPred exp) = cps_of_exps
                                      [exp]
                                      (\[exp'] -> send_to_cont
                                               (SimpleZeroPred exp')
                                               cont)
cps_of_exp cont (UnoptDiff minuend subtrahend) = cps_of_exps
                                                [minuend, subtrahend]
                                                (\[minuend', subtrahend'] -> send_to_cont
                                                                             (SimpleDiff
                                                                              minuend'
                                                                              subtrahend')
                                                                             cont)
cps_of_exp cont (UnoptIf predicate alternative consequent) = do
  cont' <- cps_of_exp cont alternative
  alt' <- cps_of_exp cont consequent
  cps_of_exps
    [predicate]
    (\[predicate'] -> CpsIf predicate' cont' alt')
cps_of_exp cont (UnoptLet var exp body) = cps_of_exp
                                          cont
                                          (UnoptCall
                                           (UnoptLambda
                                            [var]
                                            body)
                                           [exp])
cps_of_exp cont (UnoptLetrec name params proc_body body) = do
  cont_var <- get_var
  proc_body' <- cps_of_exp (SimpleVar cont_var) proc_body
  body' <- cps_of_exp cont body
  return (CpsLetrec name (params ++ [cont_var]) proc_body' body')
cps_of_exp cont (UnoptCall operator operands) = cps_of_exps
                                                (operator : operands)
                                                (\(operator' : operands') -> CpsCall
                                                                           operator'
                                                                           (operands' ++ [cont]))

simple :: Unoptimised -> Bool
simple (UnoptConst _) = True
simple (UnoptDiff m s) = simple m && simple s
simple (UnoptZeroPred n) = simple n
simple (UnoptVar _) = True
simple (UnoptLambda _ _) = True
simple _ = False
