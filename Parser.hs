module Parser where
import Control.Applicative
import Parsing
import Types

const_parser :: Parser Unoptimised
const_parser = fmap UnoptConst builtin

var_parser = fmap UnoptVar
             (some
              (sat
               (contains "qwertyuiopasdfghjklzxcvbnm_QWERTYUIOPASDFGHJKLZXCVBNM")))
  where (x : xs) `contains` y = (x == y) || (xs `contains` y)
        [] `contains` y = False

diff_parser = do
  minuend <- padded_numeric_parser <> padded_dynamic_parser
  string " - "
  subtrahend <- padded_numeric_parser <> padded_dynamic_parser
  pure (minuend `UnoptDiff` subtrahend)

zero_pred_parser = do
  string "zero?("
  value <- numeric_parser <> dynamic_parser
  char ')'
  pure (UnoptZeroPred value)

monadic_lambda_parser = do
  char 'λ'
  param <- fmap unopt_var var_parser
  char '.'
  body <- padded_parser
  pure (UnoptLambda [param] body)

polyadic_lambda_parser = do
  string "λ("
  param <- fmap unopt_var var_parser
  params <- some (string ", " >> fmap unopt_var var_parser)
  string ")."
  body <- padded_parser
  pure (UnoptLambda (param : params) body)

lambda_parser = monadic_lambda_parser <> polyadic_lambda_parser

if_parser = do
  string "if "
  predicate <- padded_numeric_parser <> padded_dynamic_parser
  string " then "
  consequent <- padded_parser
  string " else "
  alternative <- padded_parser
  pure (UnoptIf predicate consequent alternative)

let_parser = do
  string "let "
  var <- fmap unopt_var var_parser
  string " = "
  exp <- padded_parser
  string " in "
  body <- padded_parser
  pure (UnoptLet var exp body)

letrec_parser = do
  string "letrec "
  var <- fmap unopt_var var_parser
  char '('
  param <- fmap unopt_var var_parser
  params <- many (string ", " >> fmap unopt_var var_parser)
  string ") = "
  proc_body <- padded_parser
  string " in "
  body <- padded_parser
  pure (UnoptLetrec var (param : params) proc_body body)

monadic_call_parser = do
  operator <- padded_dynamic_parser <> lambda_parser
  char '('
  operand <- exp_parser
  char ')'
  pure (UnoptCall operator [operand])

polyadic_call_parser = do
  operator <- padded_dynamic_parser <> lambda_parser
  char '('
  operand <- exp_parser
  operands <- some (string ", " >> exp_parser)
  char ')'
  pure (UnoptCall operator (operand : operands))

call_parser = monadic_call_parser <> polyadic_call_parser

padded_numeric_parser = zero_pred_parser <> do
  char '('
  exp <- diff_parser
  char ')'
  pure exp

padded_dynamic_parser = var_parser <> const_parser <> do
  char '('
  exp <- if_parser <> let_parser <> letrec_parser <> call_parser
  char ')'
  pure exp

padded_parser = padded_numeric_parser <> padded_dynamic_parser <> lambda_parser

numeric_parser = const_parser <> diff_parser <> zero_pred_parser

dynamic_parser = var_parser <> if_parser <> let_parser <> letrec_parser <> call_parser

exp_parser = lambda_parser <> program_parser

program_parser :: Parser Unoptimised
program_parser = numeric_parser <> dynamic_parser
