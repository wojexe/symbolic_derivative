import Control.Monad
import Data.Functor
import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
  putStrLn "Enter math expression to be derived (possible variables: x, y, z):"
  input <- getLine

  when (hasUnmatchedParens input) $ error "The math expression you provided has unmatched parentheses"

  case parseExpression (squeeze input) of
    Left err -> putStrLn $ "Error while parsing: " ++ show err
    Right expr -> do
      putStrLn $ "Parsed expression: " ++ show expr
      putStrLn "Partial derivatives:"
      putStrLn $ "  d/dx: " ++ show (dx expr)
      putStrLn $ "  d/dy: " ++ show (dy expr)
      putStrLn $ "  d/dz: " ++ show (dz expr)

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
  showsPrec prec (Neg a) = showParen True $ showChar '-' . showsPrec 9 a
  showsPrec prec (Add a (Neg b)) = showParen (prec > 7) $ showsPrec 7 a . showString " - " . showsPrec 7 b
  showsPrec prec (Add a b) = showParen (prec > 7) $ showsPrec 7 a . showString " + " . showsPrec 7 b
  showsPrec prec (Mul a b) = showParen (prec > 8) $ showsPrec 8 a . showString " * " . showsPrec 8 b
  showsPrec prec (Div a b) = showParen (prec > 8) $ showsPrec 8 a . showString " / " . showsPrec 8 b
  showsPrec prec (Pow a b) = showParen (prec > 9) $ showsPrec 9 a . showString " ^ " . showsPrec 9 b
  showsPrec prec (Sin a) = showParen False $ showString "sin(" . shows a . showString ")"
  showsPrec prec (Cos a) = showParen False $ showString "cos(" . shows a . showString ")"
  showsPrec prec (Tan a) = showParen False $ showString "tan(" . shows a . showString ")"
  showsPrec prec (Exp a) = showParen False $ showString "exp(" . shows a . showString ")"
  showsPrec prec (Log a) = showParen False $ showString "log(" . shows a . showString ")"

dx :: (Eq a, Floating a) => Expr a -> Expr a
dx = fullSimplify . diff 'x'

dy :: (Eq a, Floating a) => Expr a -> Expr a
dy = fullSimplify . diff 'y'

dz :: (Eq a, Floating a) => Expr a -> Expr a
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

-- Helper operators
(<++>) a b = (++) <$> a <*> b

(<:>) a b = (:) <$> a <*> b

-- Parsers

parseConst :: Parser (Expr Double)
parseConst = do
  neg <- option id (char '-' $> Neg)
  value <- read <$> integer <++> decimal
  return (neg (Const value))
  where
    integer = many1 digit
    decimal = option "" $ char '.' <:> integer

parseTerm :: Parser (Expr Double)
parseTerm = parseParentheses <|> parseVar <|> parseConst

parseParentheses :: Parser (Expr Double)
parseParentheses = between (char '(') (char ')') parseFullExpr

parseVar :: Parser (Expr Double)
parseVar = Var <$> oneOf "xyz"

parseFunc :: Parser (Expr Double)
parseFunc =
  choice
    [ Sin <$> (string "sin" *> parseParentheses),
      Cos <$> (string "cos" *> parseParentheses),
      Tan <$> (string "tg" *> parseParentheses),
      Exp <$> (string "exp" *> parseParentheses),
      Log <$> (string "ln" *> parseParentheses)
    ]

parsePow :: Parser (Expr Double)
parsePow = do
  base <- parseTerm
  _ <- char '^'
  Pow base <$> parseTerm

parseNeg :: Parser (Expr Double)
parseNeg = Neg <$> (char '-' *> parseTerm)

parseAddSub :: Parser (Expr Double -> Expr Double -> Expr Double)
parseAddSub =
  choice
    [ char '+' $> Add,
      char '-' $> (\x y -> Add x (Neg y))
    ]

parseMulDiv :: Parser (Expr Double -> Expr Double -> Expr Double)
parseMulDiv =
  choice
    [ char '*' $> Mul,
      char '/' $> Div
    ]

parseExpr :: Parser (Expr Double)
parseExpr = try parsePow <|> try parseFunc <|> parseTerm

parseFullExpr :: Parser (Expr Double)
parseFullExpr = chainl1 parseExpr (parseAddSub <|> parseMulDiv)

parseExpression :: String -> Either ParseError (Expr Double)
parseExpression = parse parseFullExpr ""

--
-- SIMPLIFICATIONS
--

simplify :: (Num a, Eq a, Floating a) => Expr a -> Expr a
simplify (Neg (Neg a)) = a
simplify (Neg (Const 0)) = Const 0
simplify (Neg (Const a)) = Const (-a)
simplify (Neg (Add a b)) = Add (Neg a) (Neg b)
--
simplify (Add (Const a) (Const b)) = Const (a + b)
simplify (Add a (Const 0)) = simplify a
simplify (Add (Const 0) b) = simplify b
--
simplify (Mul (Const a) (Const b)) = Const (a * b)
simplify (Mul a (Const 1)) = simplify a
simplify (Mul (Const 1) b) = simplify b
simplify (Mul _ (Const 0)) = Const 0
simplify (Mul (Const 0) _) = Const 0
--
simplify (Mul (Add (Const a) (Const b)) (Const c)) = Add (Const (a * c)) (Const (b * c))
simplify (Mul (Add a (Const b)) (Const c)) = Add (Mul a (Const c)) (Const (b * c))
simplify (Mul (Add (Const a) b) (Const c)) = Add (Const (a * c)) (Mul b (Const c))
--
simplify (Mul (Const a) (Mul (Const b) c)) = Mul (Const $ a * b) c
simplify (Mul (Const a) (Mul b (Const c))) = Mul (Const $ a * c) b
--
simplify (Mul (Var a) (Var b))
  | a == b = Pow (Var a) (Const 2)
  | otherwise = Mul (Var a) (Var b)
simplify (Mul (Pow (Var a) b) (Var c))
  | a == c = Pow (Var a) (Add b (Const 1))
  | otherwise = Mul (Pow (Var a) b) (Var c)
simplify (Mul (Var a) (Pow (Var b) c))
  | a == b = Pow (Var a) (Add c (Const 1))
  | otherwise = Mul (Var a) (Pow (Var b) c)
--
simplify (Div (Const a) (Const b)) = Const (a / b)
simplify (Div (Const 0) b) = Const 0
simplify (Div a (Const 0)) = error "Division by zero"
simplify (Div a (Const 1)) = simplify a
--
simplify (Pow (Const 0) (Const 0)) = error "Undefined symbol 0^0"
simplify (Pow a (Const 0)) = Const 1
simplify (Pow a (Const 1)) = a
simplify (Pow (Const a) (Const b)) = Const $ a ** b
--
simplify (Log (Const 0)) = error "ln(0) is undefined"
simplify (Log (Const 1)) = Const 0
--
simplify (Neg a) = Neg $ simplify a
simplify (Add a b)
  | a == b = Mul (Const 2) (simplify a)
  | otherwise = Add (simplify a) (simplify b)
simplify (Mul a b) = simplify a `Mul` simplify b
simplify (Div a b) = simplify a `Div` simplify b
simplify (Pow a b) = simplify a `Pow` simplify b
simplify (Sin a) = Sin $ simplify a
simplify (Cos a) = Cos $ simplify a
simplify (Tan a) = Tan $ simplify a
simplify (Exp a) = Exp $ simplify a
simplify (Log a) = Log $ simplify a
--
simplify x = x

fullSimplify e = helper e (Const 0)
  where
    helper curr prev
      | curr == prev = curr
      | otherwise = helper next curr
      where
        next = simplify curr

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

--
-- EXAMPLES
--

-- sin (1/x) * exp (y ^ 3 - 2)
sampleExpr :: Expr Float
sampleExpr = Mul (Sin $ Div (Const 1) (Var 'x')) (Exp $ Add (Pow (Var 'y') (Const 3)) (Neg (Const 2)))

sampleExpr2 :: Expr Float
sampleExpr2 = Const 3 `Add` Neg (Const 2) `Mul` (Neg (Const 10) `Mul` Var 'x')

sampleExpr3 :: Expr Float
sampleExpr3 = Mul (Add (Const 2) (Neg (Const 5))) (Const 10)

ip1 :: Expr Float
ip1 =
  Add
    ( Mul
        (Add (Const 2) (Neg (Var 'x')))
        (Mul (Const 4) (Var 'y'))
    )
    ( Neg
        ( Mul
            (Pow (Const 2) (Const 4))
            (Mul (Const 6) (Var 'z'))
        )
    )

am21 :: Expr Float
am21 = Mul (Mul (Pow (Var 'x') (Const 3)) (Pow (Var 'y') (Const 2))) (Var 'z')

--
-- TESTS
--

-- main :: IO ()
-- main = do
--   let variable = 'x'

--   -- Test case 1: Derivative of a simple expression (x^2 + sin(x))
--   let expr1 = Add (Pow (Var 'x') (Const 2)) (Sin (Var 'x'))
--   putStrLn $ "Test Case 1 - Original expression: " ++ show expr1
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr1)

--   -- Test case 2: Derivative involving multiplication and division (x*sin(x))
--   let expr2 = Mul (Var 'x') (Sin (Var 'x'))
--   putStrLn $ "\nTest Case 2 - Original expression: " ++ show (expr2 :: Expr Double)
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr2)

--   -- Test case 3: Derivative involving exponentiation (e^(x^2))
--   let expr3 = Exp (Pow (Var 'x') (Const 2))
--   putStrLn $ "\nTest Case 3 - Original expression: " ++ show expr3
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr3)

--   -- Test case 4: Derivative involving logarithm (ln(x))
--   let expr4 = Log (Var 'x')
--   putStrLn $ "\nTest Case 4 - Original expression: " ++ show (expr4 :: Expr Double)
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr4)

--   -- Test case 5: Derivative involving multiple operations (tan(x) + e^(x^2))
--   let expr5 = Add (Tan (Var 'x')) (Exp (Pow (Var 'x') (Const 2)))
--   putStrLn $ "\nTest Case 5 - Original expression: " ++ show expr5
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr5)

--   -- Test case 6: Derivative involving nested functions (sin(cos(x)))
--   let expr6 = Sin (Cos (Var 'x'))
--   putStrLn $ "\nTest Case 6 - Original expression: " ++ show (expr6 :: Expr Double)
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr6)

--   -- Test case 7: Derivative involving nested exponentiation (e^(x^(x^2)))
--   let expr7 = Exp (Pow (Var 'x') (Pow (Var 'x') (Const 2)))
--   putStrLn $ "\nTest Case 7 - Original expression: " ++ show expr7
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr7)

--   -- Test case 8: Derivative involving a fraction with complex numerator and denominator
--   let expr8 = Div (Add (Mul (Var 'x') (Sin (Var 'x'))) (Exp (Var 'x'))) (Var 'x')
--   putStrLn $ "\nTest Case 8 - Original expression: " ++ show (expr8 :: Expr Double)
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr8)

--   -- Test case 9: Derivative involving a combination of functions (tan(x^2) * ln(cos(x)))
--   let expr9 = Mul (Tan (Pow (Var 'x') (Const 2))) (Log (Cos (Var 'x')))
--   putStrLn $ "\nTest Case 9 - Original expression: " ++ show expr9
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr9)

--   -- Test case 10: Derivative involving a combination of functions and constants (2 * x * e^(x^2))
--   let expr10 = Mul (Const 2) (Mul (Var 'x') (Exp (Pow (Var 'x') (Const 2))))
--   putStrLn $ "\nTest Case 10 - Original expression: " ++ show expr10
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr10)

--   -- Test case 11: Derivative involving a combination of trigonometric and exponential functions
--   let expr11 = Mul (Sin (Var 'x')) (Exp (Mul (Var 'x') (Cos (Var 'x'))))
--   putStrLn $ "\nTest Case 11 - Original expression: " ++ show (expr11 :: Expr Double)
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr11)

--   -- Test case 12: Derivative involving a more intricate combination of functions and constants
--   let expr12 = Div (Exp (Mul (Var 'x') (Var 'x'))) (Add (Sin (Var 'x')) (Mul (Const 2) (Cos (Var 'x'))))
--   putStrLn $ "\nTest Case 12 - Original expression: " ++ show expr12
--   putStrLn $ "Derivative with respect to '" ++ [variable] ++ "': " ++ show (dx expr12)

--   let expr13 = Mul (Sin (Var 'x')) (Exp (Mul (Var 'y') (Cos (Var 'z'))))
--   putStrLn $ "\nTest Case 13 - Original expression: " ++ show (expr13 :: Expr Double)
--   putStrLn $ "Derivative with respect to 'x': " ++ show (dx expr13)
--   putStrLn $ "Derivative with respect to 'y': " ++ show (dy expr13)
--   putStrLn $ "Derivative with respect to 'z': " ++ show (dz expr13)
