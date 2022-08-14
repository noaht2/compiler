{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Parser where
import Control.Applicative
import Parsing
import Types

data Tree a = Leaf {leaf :: a} | Branch {branches :: [Tree a]} deriving (Eq,
                                                                         Read,
                                                                         Show,
                                                                         Functor,
                                                                         Foldable)

contains :: Eq a => [a] -> a -> Bool
(x : xs) `contains` y = (x == y) || (xs `contains` y)
[] `contains` y = False

allowed_characters
  =
  "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890−×÷“”‘’ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω¬!£$%^&*-_=+/*;:@#~,<.>?"

atom_parser :: Parser (Tree String)
atom_parser = fmap Leaf (some (sat (contains allowed_characters)))

whitespace :: Parser String
whitespace = some (sat (contains "\t\n\v\f\r "))

list_parser :: Parser (Tree String)
list_parser = do
  char '('
  head <- tree_parser
  tail <- many (whitespace >> tree_parser)
  char ')'
  pure (Branch (head : tail))

tree_parser :: Parser (Tree String)
tree_parser = atom_parser <|> list_parser

parse_tree :: Tree String -> Unoptimised
parse_tree (Leaf s) = case reads s of
                        [(n, "")] -> UnoptConst n
                        _ -> UnoptVar s
parse_tree (Branch [Leaf "−", t1, t2]) = (parse_tree t1) `UnoptDiff` (parse_tree t2)
parse_tree (Branch [Leaf "-", t1, t2]) = (parse_tree t1) `UnoptDiff` (parse_tree t2)
parse_tree (Branch [Leaf "zero?", t1]) = UnoptZeroPred (parse_tree t1)
parse_tree (Branch [Leaf "λ", Branch ps, b]) = UnoptLambda (map leaf ps) (parse_tree b)
parse_tree (Branch [Leaf "lambda", Branch ps, b]) = UnoptLambda (map leaf ps) (parse_tree b)
parse_tree (Branch [Leaf "if", p, c, a]) = UnoptIf (parse_tree p) (parse_tree c) (parse_tree a)
parse_tree (Branch [Leaf "let", Leaf v, e, b]) = UnoptLet v (parse_tree e) (parse_tree b)
parse_tree (Branch [Leaf "letrec", Leaf n, Branch ps, pb, b]) =
  UnoptLetrec n (map leaf ps) (parse_tree pb) (parse_tree b)
parse_tree (Branch (operator : operands)) =
  UnoptCall (parse_tree operator) (map parse_tree operands)

program_parser :: Parser Unoptimised
program_parser = fmap parse_tree tree_parser
