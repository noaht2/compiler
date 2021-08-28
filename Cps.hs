module Cps (convert) where
import Data.List
import System.IO.Unsafe
import Types
import Unsafe

send_to_cont :: SimpleCps -> SimpleCps -> Cps
send_to_cont exp cont = CpsCall cont [exp]

convert :: Unoptimised -> Cps
convert prog = cps_of_exps [prog] (SimpleCps . (!! 0))

replace :: Eq a => [a] -> Int -> a -> [a]
replace xs i y = acc xs i y
  where acc (x : xs) 0 y = y : xs
        acc (x : xs) j y = x : acc xs (j - 1) y

cps_of_simple_exp :: Unoptimised -> SimpleCps
cps_of_simple_exp (UnoptConst n) = SimpleConst n
cps_of_simple_exp (UnoptVar v) = SimpleVar v
cps_of_simple_exp (UnoptDiff m s) = SimpleDiff
                                    (cps_of_simple_exp m)
                                    (cps_of_simple_exp s)
cps_of_simple_exp (UnoptZeroPred e) = SimpleZeroPred
                                      (cps_of_simple_exp e)
cps_of_simple_exp (UnoptLambda ps b) = SimpleLambda -- sus
                                       (ps ++ ["cont"])
                                       (cps_of_exp (SimpleVar "cont") b)

cps_of_exps :: [Unoptimised] -> ([SimpleCps] -> Cps) -> Cps
{-# NOINLINE cps_of_exps #-}
cps_of_exps exps builder = cps_of_rest exps
  where
    cps_of_rest exps
      = let pos = findIndex (not . simple) exps in
          case pos of
            Nothing -> builder (map cps_of_simple_exp exps)
            Just pos' -> let var = unsafePerformIO get_identifier in
                           cps_of_exp
                           (SimpleLambda -- sus
                            [var]
                            (cps_of_rest
                             (replace exps pos' (UnoptVar var))))
                           (exps !! pos')

cps_of_exp :: SimpleCps -> Unoptimised -> Cps
cps_of_exp cont (UnoptConst n) = send_to_cont (SimpleConst n) cont
cps_of_exp cont (UnoptVar var) = send_to_cont (SimpleVar var) cont
cps_of_exp cont (UnoptLambda params body) = send_to_cont
                                            (SimpleLambda
                                             (params ++ ["cont"])
                                             (cps_of_exp
                                              (SimpleVar "cont")
                                              body))
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
cps_of_exp cont (UnoptIf predicate alternative consequent) = cps_of_exps
                                                             [predicate]
                                                             (\[predicate'] -> CpsIf
                                                                               predicate'
                                                                               (cps_of_exp
                                                                                cont
                                                                                alternative)
                                                                               (cps_of_exp
                                                                                cont
                                                                                consequent))
cps_of_exp cont (UnoptLet var exp body) = cps_of_exp
                                          cont
                                          (UnoptCall
                                           (UnoptLambda
                                            [var]
                                            body)
                                           [exp])
cps_of_exp cont (UnoptLetrec name params proc_body body) = CpsLetrec
                                                           name
                                                           (params ++ ["cont"])
                                                           (cps_of_exp
                                                            (SimpleVar "cont")
                                                            proc_body)
                                                           (cps_of_exp
                                                            cont
                                                            body)
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
