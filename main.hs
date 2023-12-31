import Control.Monad
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as T

main :: IO ()
main = do
  putStrLn "Enter math expression to be derived (possible variables: x, y, z):"
  input <- getLine

  when (hasUnmatchedParens input) $ error "The math expression you provided has unmatched parentheses"

  case parseExpr (squeeze input) of
    Left err -> putStrLn $ "Error while parsing: " ++ show err
    Right expr -> do
      putStrLn $ "Parsed expression: " ++ show expr

      let simplified = fullSimplify expr

      putStrLn $ "Simplified expression: " ++ show simplified

      putStrLn "Partial derivatives:"
      putStrLn $ "  d/dx: " ++ show (dx simplified)
      putStrLn $ "  d/dy: " ++ show (dy simplified)
      putStrLn $ "  d/dz: " ++ show (dz simplified)

squeeze :: String -> String
squeeze = filter (/= ' ')

hasUnmatchedParens :: String -> Bool
hasUnmatchedParens str = left /= right
  where
    left = length $ filter (== '(') str
    right = length $ filter (== ')') str

--
-- IMPLEMENTATION
--

data Expr a
  = Var Char
  | Const a
  | Neg (Expr a)
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)
  | Sin (Expr a)
  | Cos (Expr a)
  | Tan (Expr a)
  | Exp (Expr a)
  | Log (Expr a)
  deriving (Eq)

instance (Show a) => Show (Expr a) where
  showsPrec prec (Var ch) = showChar ch
  showsPrec prec (Const a) = showsPrec prec a
  showsPrec 0 (Neg a) = showChar '-' . showsPrec 9 a -- Neg is the outermost
  showsPrec prec (Neg a) = showParen True $ showChar '-' . showsPrec 9 a
  showsPrec prec (Add a (Neg b)) = showParen (prec > 7) $ showsPrec 7 a . showString " - " . showsPrec 7 b
  showsPrec prec (Add a b) = showParen (prec > 7) $ showsPrec 7 a . showString " + " . showsPrec 7 b
  showsPrec prec (Mul a b) = showParen (prec > 8) $ showsPrec 8 a . showString " * " . showsPrec 8 b
  showsPrec prec (Div a b) = showParen (prec > 8) $ showsPrec 8 a . showString " / " . showsPrec 8 b
  showsPrec prec (Pow a b) = showParen (prec > 9) $ showsPrec 9 a . showString " ^ " . showsPrec 9 b
  showsPrec prec (Sin a) = showParen False $ showString "sin(" . shows a . showString ")"
  showsPrec prec (Cos a) = showParen False $ showString "cos(" . shows a . showString ")"
  showsPrec prec (Tan a) = showParen False $ showString "tg(" . shows a . showString ")"
  showsPrec prec (Exp a) = showParen False $ showString "exp(" . shows a . showString ")"
  showsPrec prec (Log a) = showParen False $ showString "ln(" . shows a . showString ")"

dx :: (Ord a, Floating a) => Expr a -> Expr a
dx = fullSimplify . diff 'x'

dy :: (Ord a, Floating a) => Expr a -> Expr a
dy = fullSimplify . diff 'y'

dz :: (Ord a, Floating a) => Expr a -> Expr a
dz = fullSimplify . diff 'z'

diff :: (Floating a) => Char -> Expr a -> Expr a
-- znak `c` traktowany jako zmienna
diff c (Const _) = Const 0
diff c (Var ch)
  | c == ch = Const 1
  | otherwise = Const 0
diff c (Neg a) = Neg (diff c a)
diff c (Add a b) = Add (diff c a) (diff c b)
-- f(x) * g(x) -> f'(x) * g(x) + f(x) * g'(x)
diff c (Mul a b) = Add (Mul (diff c a) b) (Mul a (diff c b))
-- f(x) / g(x) -> ( f'(x) * g(x) - f(x) * g'(x) ) / g^2(x)
diff c (Div a b) = Div (Add (Mul (diff c a) b) (Neg (Mul a (diff c b)))) (Pow b (Const 2))
-- a^f(x) -> a^f(x) * log(a) * f'(x)
diff c (Pow (Const a) b) = Mul (Mul (Pow (Const a) b) (Const (log a))) (diff c b)
-- f(x)^b -> b * f(x) ^ ( b - 1 ) * f'(x)
diff c (Pow a (Const b)) = Mul (Mul (Const b) (Pow a (Const (b - 1)))) (diff c a)
-- f(x) ^ g(x) -> f(x) ^ (g(x) - 1) * (g(x) * f'(x) + f(x) * log(f(x)) * g'(x))
diff c (Pow a b) = Mul (Pow a (Add b (Neg (Const 1)))) (Add (Mul b (diff c a)) (Mul a (Mul (Log a) (diff c b))))
-- sin(f(x)) -> f'(x) * cos(f(x))
diff c (Sin a) = Mul (diff c a) (Cos a)
-- cos(f(x)) -> -( f'(x) * sin(f(x)) )
diff c (Cos a) = Neg $ Mul (diff c a) (Sin a)
-- tg(f(x)) -> f'(x) / ( cos^2(f(x)) )
diff c (Tan a) = Div (diff c a) (Pow (Cos a) (Const 2))
-- exp(f(x)) -> f'(x) * exp(f(x))
diff c (Exp a) = Mul (diff c a) (Exp a)
-- log(f(x)) -> f'(x) / f(x)
diff c (Log a) = Div (diff c a) a

--
-- PARSING
--

lexer = T.makeTokenParser emptyDef

parens = T.parens lexer

reservedOp = T.reservedOp lexer

reserved = T.reserved lexer

number = do
  num <- T.naturalOrFloat lexer
  return $ case num of
    Left natural -> fromInteger natural
    Right floating -> floating

function name constructor =
  reserved name *> fmap constructor (parens expr)

expr :: Parser (Expr Double)
expr = buildExpressionParser table terms

table =
  [ [Prefix (reservedOp "-" >> return Neg)],
    [Infix (reservedOp "^" >> return Pow) AssocRight],
    [Infix (reservedOp "*" >> return Mul) AssocLeft, Infix (reservedOp "/" >> return Div) AssocLeft],
    [Infix (reservedOp "+" >> return Add) AssocLeft, Infix (reservedOp "-" >> return (\a b -> Add a (Neg b))) AssocLeft]
  ]

terms =
  parens expr
    <|> fmap Var (oneOf "xyz")
    <|> fmap Const number
    <|> function "sin" Sin
    <|> function "cos" Cos
    <|> function "tg" Tan
    <|> function "exp" Exp
    <|> function "ln" Log

parseExpr :: String -> Either ParseError (Expr Double)
parseExpr = parse (expr <* eof) "<stdin>"

--
-- SIMPLIFICATIONS
--

s :: (Num a, Ord a, Floating a) => Expr a -> Expr a
s (Const a)
  | a < 0 = Neg (Const (-a))
  | otherwise = Const a
--
s (Neg (Neg a)) = a
s (Neg (Const 0)) = Const 0
s (Add (Neg a) (Neg b)) = Neg $ Add a b
--
s (Mul (Neg a) (Neg b)) = Mul a b
s (Mul (Neg a) b) = Neg $ Mul a b
s (Mul a (Neg b)) = Neg $ Mul a b
--
s (Div (Neg a) (Neg b)) = Div a b
s (Div (Neg a) b) = Neg $ Div a b
s (Div a (Neg b)) = Neg $ Div a b
--
s (Add (Const a) (Const b)) = Const (a + b)
s (Add (Const a) (Neg (Const b))) = Const (a - b)
s (Add a (Const 0)) = a
s (Add (Const 0) b) = b
--
s (Mul (Const a) (Const b)) = Const (a * b)
s (Mul a (Const b)) = Mul (Const b) a
s (Mul a (Mul (Const b) c)) = Mul (Const b) (s $ Mul a c)
s (Mul (Const 1) b) = b
s (Mul (Const 0) _) = Const 0
--
s (Mul (Const c) (Add a b)) = Add (s $ Mul (Const c) a) (s $ Mul (Const c) b)
--
s (Mul (Pow a b) (Pow c d))
  | a == c = Pow (s a) (s $ Add b d)
s (Mul (Pow a b) c)
  | a == c = Pow (s a) (s $ Add b (Const 1))
s (Mul a (Pow b c))
  | a == b = Pow (s a) (s $ Add c (Const 1))
--
s (Mul a (Div b c)) = Div (Mul a b) c
s (Mul (Div a b) c) = Div (Mul a c) b
--
s (Div (Const a) (Const b)) = Const (a / b)
s (Div (Const 0) b) = Const 0
s (Div a (Const 0)) = error "Division by zero"
s (Div a (Const 1)) = a
--
s (Div (Pow a b) (Pow c d))
  | a == c = Pow (s a) (s $ Add b (Neg d))
s (Div (Pow a b) c)
  | a == c = Pow (s a) (s $ Add b (Neg $ Const 1))
s (Div a (Pow b c))
  | a == b = Pow (s a) (s $ Add (Const 1) (Neg c))
--
s (Pow (Const 0) (Const 0)) = error "Undefined symbol 0^0"
s (Pow a (Const 0)) = Const 1
s (Pow a (Const 1)) = s a
s (Pow a (Neg b)) = Const 1 `Div` Pow a b
s (Pow (Const a) (Const b)) = Const $ a ** b
s (Pow (Pow a (Const b)) (Const c)) = Pow a (Const $ b * c)
--
s (Log (Const 0)) = error "ln(0) is undefined"
s (Log (Const 1)) = Const 0
--
s (Neg a) = Neg $ s a
s (Add a b)
  | a == b = Mul (Const 2) (s a)
  | otherwise = s a `Add` s b
s (Mul a b)
  | a == b = s a `Pow` Const 2
  | otherwise = s a `Mul` s b
s (Div a b)
  | a == b = Const 1
  | otherwise = s a `Div` s b
s (Pow a b) = s a `Pow` s b
s (Sin a) = Sin $ s a
s (Cos a) = Cos $ s a
s (Tan a) = Tan $ s a
s (Exp a) = Exp $ s a
s (Log a) = Log $ s a
--
s x = x

fullSimplify e = helper e (Const 0)
  where
    helper curr prev
      | curr == prev = curr
      | otherwise = helper next curr
      where
        next = s curr

--
-- VALUE CALCULATION
--

myf :: (Floating a) => Char -> a
myf 'x' = 1
myf 'y' = 2
myf 'z' = 3

calc :: (Floating a) => (Char -> a) -> Expr a -> a
calc f (Var c) = f c
calc f (Const a) = a
calc f (Neg a) = -(calc f a)
calc f (Add a b) = calc f a + calc f b
calc f (Mul a b) = calc f a * calc f b
calc f (Div a b) = calc f a / calc f b
calc f (Pow a b) = calc f a ** calc f b
calc f (Sin a) = sin $ calc f a
calc f (Cos a) = cos $ calc f a
calc f (Tan a) = tan $ calc f a
calc f (Exp a) = exp $ calc f a
calc f (Log a) = log $ calc f a
