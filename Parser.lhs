> module Parser where

> import Control.Monad
> import Data.Char
> import qualified Control.Applicative as CA
> infixr 5 +++

> newtype Parser a = Parser (String -> Maybe (a,String))

> instance Applicative Parser where
>     pure  = return
>     (<*>) = ap
 
> instance Functor Parser where
>       fmap  = liftM

> failure :: Parser a
> failure = Parser(\inp -> Nothing)

> parse :: Parser a -> String -> Maybe (a,String)
> parse (Parser p) inp = p inp

> instance Monad Parser where
>     return v = Parser (\inp -> Just (v,inp))
>     p >>= f = Parser (\inp -> case parse p inp of
>                             Nothing -> Nothing
>                             Just (v,out) -> parse (f v) out)
  
> item :: Parser Char
> item = Parser (\inp -> case inp of
>                     [] -> Nothing
>                     (x:xs) -> Just (x,xs))

> (+++) :: Parser a -> Parser a -> Parser a
> p +++ q = Parser (\inp -> case parse p inp of 
>                     Nothing -> parse q inp
>                     Just (v,out) -> Just (v,out))

> sat :: (Char -> Bool) -> Parser Char
> sat p = do x <- item
>            if p x then return x else failure

> digit :: Parser Char
> digit = sat isDigit

> lower :: Parser Char
> lower = sat isLower

> upper :: Parser Char
> upper = sat isUpper

> letter :: Parser Char
> letter = sat isAlpha

> alphanum :: Parser Char
> alphanum = sat isAlphaNum

> char :: Char -> Parser Char
> char ch = sat (== ch)

> string :: String -> Parser String
> string "" = return ""
> string (x:xs) = do char x
>                    string xs
>                    return (x:xs)

> many :: Parser a -> Parser [a]
> many p = many1 p +++ return []

> many1 :: Parser a -> Parser [a]
> many1 p = do v <- p
>              vs <- many p
>              return (v:vs) 

> ident :: Parser String
> ident = do x <- lower
>            xs <- many alphanum
>            return (x:xs)

> nat :: Parser Int
> nat = do xs <- many1 digit
>          return (read xs)

> int :: Parser Int
> int = neg +++ nat
>     where neg = do char '-'
>                    xs <- many1 digit
>                    return (-(read xs))

> space :: Parser ()
> space = do many(sat isSpace)
>            return ()

> token :: Parser a -> Parser a
> token p = do space
>              v <- p
>              space
>              return v

> identifier :: Parser String
> identifier = token ident

> natural :: Parser Int
> natural = token nat

> integer :: Parser Int
> integer = token int

> symbol :: String -> Parser String
> symbol xs = token (string xs)

> data Op = Add | Sub | Mul | Div | Exp deriving (Show)

> data ExprTree a = Empty | Num a | ExprTree Op (ExprTree a) (ExprTree a) deriving (Show)

> expr :: Parser (ExprTree Int)
> expr = exprAcc Empty

> exprAcc :: ExprTree Int -> Parser (ExprTree Int)
> exprAcc Empty = (do v <- term
>                     res <- exprAcc v
>                     return res)
>                  +++
>                 (do v <- term
>                     return v)
> exprAcc e = (do symbol "+"
>                 v <- term
>                 res <- exprAcc (ExprTree Add e v)
>                 return res)
>              +++
>             (do symbol "-"
>                 v <- term
>                 res <- exprAcc (ExprTree Sub e v)
>                 return res)
>              +++
>             (do return e)

> term :: Parser (ExprTree Int)
> term = termAcc Empty

> termAcc :: ExprTree Int -> Parser (ExprTree Int)
> termAcc Empty = (do v <- factor
>                     res <- termAcc v
>                     return res)
>                  +++
>                 (do v <- factor
>                     return v)
> termAcc e = (do symbol "*"
>                 v <- factor
>                 res <- termAcc (ExprTree Mul e v)
>                 return res)
>              +++
>             (do symbol "/"
>                 v <- factor
>                 res <- termAcc (ExprTree Div e v)
>                 return res)
>              +++
>             (do return e)

> factor :: Parser (ExprTree Int)
> factor = (do e <- expo
>              symbol "^"
>              f <- factor
>              return (ExprTree Exp e f))
>          +++
>          (do e <- expo
>              return e)

> expo :: Parser (ExprTree Int)
> expo = (do symbol "("
>            v <- expr
>            symbol ")"
>            return v)
>       +++
>       (do v <- integer
>           return (Num v))

> eval :: String -> Int
> eval input = case parse expr input of
>               Nothing -> error "Invalid input"
>               Just (tree,"") -> evalParseTree tree
>               Just (_,out) -> error ("unused input" ++ out)

> evalParseTree :: ExprTree Int -> Int
> evalParseTree (Num x) = x
> evalParseTree (ExprTree op lt rt) = apply op (evalParseTree lt) (evalParseTree rt)

> apply :: Op -> Int -> Int -> Int
> apply Add = (+)
> apply Sub = (-)
> apply Mul = (*)
> apply Div = div
> apply Exp = (^)
