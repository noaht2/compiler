{-# LANGUAGE DeriveFunctor, ApplicativeDo #-}
module Parsing where
import Control.Monad
import Control.Applicative
import Control.Monad.Fix

newtype Parser a = Parser {parse :: String -> [(a, String)]} deriving Functor

apply_parser (Parser f) t = fst (head (filter ((== "") . snd) (f t)))

instance Applicative Parser where
  pure = Parser . ((.) pure) . (,)
  p <*> q = Parser (\s -> [(y, s''')
                           | (f, s') <- parse p s,
                             (x, s'') <- parse q s',
                             (y, s''') <- parse (pure (f x)) s''])

instance Monad Parser where
  Parser x >>= f = Parser (\y -> let parsed = x y
                                 in concat (map (parse . f . fst) parsed <*> fmap snd parsed))

instance Semigroup (Parser a) where
  Parser p <> Parser q = Parser (\x -> p x ++ q x)

instance Monoid (Parser a) where
  mempty = Parser (const [])

instance Alternative Parser where
  empty = mempty
  Parser p <|> Parser q = Parser (\x -> if null (p x) then q x else p x)

instance MonadPlus Parser

instance MonadFail Parser where
  fail _ = empty

getc :: Parser Char
getc = Parser f
  where f "" = []
        f (c : cs) = [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- getc
  if p c then pure c else empty

char :: Char -> Parser ()
char c = sat (== c) >> pure ()

string :: String -> Parser ()
string "" = pure ()
string (c : cs) = char c >> string cs

builtin :: Read a => Parser a
builtin = Parser reads
