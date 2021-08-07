module Types where

type IntT = Int

data Unoptimised = UnoptConst IntT
                 | UnoptVar String
                 | UnoptDiff {unopt_minuend :: Unoptimised,
                              unopt_subtrahend :: Unoptimised}
                 | UnoptZeroPred Unoptimised
                 | UnoptLambda {unopt_lambda_params :: [String],
                                unopt_lambda_body :: Unoptimised}
                 | UnoptIf {unopt_predicate :: Unoptimised,
                            unopt_consequent :: Unoptimised,
                            unopt_alternative :: Unoptimised}
                 | UnoptLet {unopt_let_var :: String,
                             unopt_let_exp :: Unoptimised,
                             unopt_let_body :: Unoptimised}
                 | UnoptLetrec {unopt_letrec_name :: String,
                                unopt_letrec_params :: [String],
                                unopt_letrec_proc_body :: Unoptimised,
                                unopt_letrec_body :: Unoptimised}
                 | UnoptCall {unopt_operator :: Unoptimised,
                              unopt_operands :: [Unoptimised]}
                 deriving (Show, Read, Eq)

data SimpleCps = SimpleConst IntT
               | SimpleLambda {simple_lambda_params :: [String],
                               simple_lambda_body :: Cps}
               | SimpleVar String
               | SimpleDiff {simple_minuend :: SimpleCps,
                             simple_subtrahend :: SimpleCps}
               | SimpleZeroPred SimpleCps
               deriving (Show, Read, Eq)

data Cps = SimpleCps SimpleCps
         | CpsLet {cps_let_var :: String,
                   cps_let_simple :: SimpleCps,
                   cps_let_body :: Cps}
         | CpsLetrec {cps_letrec_name :: String,
                      cps_letrec_params :: [String],
                      cps_letrec_proc_body :: Cps,
                      cps_letrec_body :: Cps}
         | CpsIf {cps_predicate :: SimpleCps,
                  cps_consequent :: Cps,
                  cps_alternative :: Cps}
         | CpsCall {cps_operator :: SimpleCps,
                    cps_operands :: [SimpleCps]}
         deriving (Show, Read, Eq)

-- data Literal = LiteralInt IntT
--              | LiteralLambda {literal_lambda_params :: [String],
--                               literal_lambda_body :: Cps,
--                               literal_lambda_env :: Environment}
--              deriving (Show, Read, Eq)

-- data EnvironmentFrame = SimpleFrame String Literal
--                       | RecursiveFrame String [String] Cps
--                       deriving (Show, Read, Eq)

-- type Environment = [EnvironmentFrame]

data UnrolledInstruction = UnrolledReturn IntT
                         | UnrolledCreateProc [String] UnrolledCode
                         | UnrolledApplyEnv String
                         | UnrolledDiff UnrolledInstruction UnrolledInstruction
                         | UnrolledZeroPred UnrolledInstruction
                         | UnrolledExtendEnv String UnrolledInstruction
                         | UnrolledExtendEnvRec {unrolled_letrec_name :: String,
                                                 unrolled_letrec_params :: [String],
                                                 unrolled_letrec_proc_body :: UnrolledCode}
                         | UnrolledIf {unrolled_predicate :: UnrolledInstruction,
                                       unrolled_consequent :: [UnrolledInstruction],
                                       unrolled_alternative :: [UnrolledInstruction]}
                         | UnrolledApplication UnrolledInstruction [UnrolledInstruction]
                         deriving (Show, Read, Eq)
                                

type UnrolledCode = [UnrolledInstruction]
