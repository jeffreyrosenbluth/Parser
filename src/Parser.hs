{-# LANGUAGE FlexibleInstances #-}

module Parser where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

--------------------------------------------------------------------------------

newtype Parser s a = Parser { parse :: s -> Either String (a, s) }

instance Functor (Parser s) where
  fmap f p = Parser $ \s ->
    let r = parse p s
    in  fmap (\(b, t) -> (f b, t)) r

instance Applicative (Parser s) where
  pure a  = Parser $ \s -> Right (a, s)
  f <*> p = Parser $ \s -> do
    (g, t) <- parse f s
    (q, u) <- parse p t
    return (g q, u)

instance Alternative (Parser s) where
  empty   = Parser $ const (Left "Fails")
  p <|> q = Parser $ \s ->
    case parse p s of
      Left e  -> parse q s
      Right r -> Right r

instance Monad (Parser s) where
  return  = pure
  p >>= f = Parser $ \s -> do
    (a, t) <- parse p s
    parse (f a) t

--------------------------------------------------------------------------------

class HasChar s where
  item :: s -> Maybe (Char, s)

instance HasChar String where
  item ""     = Nothing
  item (x:xs) = Just (x, xs)

instance HasChar T.Text where
  item = T.uncons

instance HasChar LT.Text where
  item = LT.uncons

instance HasChar BS.ByteString where
  item = BS.uncons

instance HasChar LBS.ByteString where
  item = LBS.uncons

satisfy :: HasChar s => (Char -> Bool) -> Parser s Char
satisfy pred = Parser $ \s ->
  case item s of
    Nothing     -> Left "No characters remaining"
    Just (c, t) -> if pred c then
                     Right (c, t)
                   else
                     Left "First character does not satisfy the predicate"

oneOf :: HasChar s => [Char] -> Parser s Char
oneOf s = satisfy $ flip elem s

chainl1 :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl1 p op = p >>= rest
  where
    rest a = (do
      f <- op
      b <- p
      rest (f a b))
      <|> return a

chainl :: Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl p op a = chainl1 p op <|> pure a

char :: HasChar s => Char -> Parser s Char
char c = satisfy (c ==)

natural :: HasChar s => Parser s Integer
natural = read <$> some (satisfy isDigit)

string :: HasChar s => String -> Parser s String
string cs = sequence $ map char cs

spaces :: HasChar s => Parser s String
spaces = many $ oneOf " \n\r"

token :: HasChar s => Parser s a -> Parser s a
token p = p <* spaces

reserved :: HasChar s => String -> Parser s String
reserved = token . string

digit :: HasChar s => Parser s Char
digit = satisfy isDigit

number :: HasChar s => Parser s Int
number = f <$> (string "-" <|> pure "") <*> some digit
  where f a b = read $ a ++ b

parens :: HasChar s => Parser s a -> Parser s a
parens m = reserved "(" *> m <* reserved ")"
