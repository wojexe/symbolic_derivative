main :: IO ()
main = do
  putStrLn "Enter math expression to be derived (possible variables: x, y, z):"
  input <- getLine

  if hasUnmatchedParens input
    then error "The math expression you provided has unmatched parentheses"
    else return ()

  case parse (squeeze input) of
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
diff c (Neg f) = Neg (diff c f)
-- f(x) + g(x) -> f'(x) + g'(x)
diff c (Add f g) = Add (diff c f) (diff c g)
-- f(x) * g(x) -> f'(x) * g(x) + f(x) * g'(x)
diff c (Mul f g) = Add (Mul (diff c f) g) (Mul f (diff c g))
-- f(x) / g(x) -> ( f'(x) * g(x) - f(x) * g'(x) ) / g^2(x)
diff c (Div f g) = Div (Add (Mul (diff c f) g) (Neg (Mul f (diff c g)))) (Pow g (Const 2))
-- a^f(x) -> a^f(x) * log(a) * f'(x)
diff c (Pow (Const a) f) = Mul (Mul (Pow (Const a) f) (Const (log a))) (diff c f)
-- f(x)^b -> b * f(x) ^ ( b - 1 ) * f'(x)
diff c (Pow f (Const b)) = Mul (Mul (Const b) (Pow f (Const (b - 1)))) (diff c f)
-- f(x) ^ g(x) -> f(x) ^ (g(x) - 1) * (g(x) * f'(x) + f(x) * log(f(x)) * g'(x))
diff c (Pow f g) = Mul (Pow f (Add g (Neg (Const 1)))) (Add (Mul g (diff c f)) (Mul f (Mul (Log f) (diff c g))))
-- sin(f(x)) -> f'(x) * cos(f(x))
diff c (Sin f) = Mul (diff c f) (Cos f)
-- cos(f(x)) -> -( f'(x) * sin(f(x)) )
diff c (Cos f) = Neg $ Mul (diff c f) (Sin f)
-- tg(f(x)) -> f'(x) / ( cos^2(f(x)) )
diff c (Tan f) = Div (diff c f) (Pow (Cos f) (Const 2))
-- exp(f(x)) -> f'(x) * exp(f(x))
diff c (Exp f) = Mul (diff c f) (Exp f)
-- log(f(x)) -> f'(x) / f(x)
diff c (Log f) = Div (diff c f) f

--
-- TOKENIZATION
--

operators = "+-*/^"

variables = "xyz"

digits = "0123456789"

characters = "abcdefghijklmnopqrstuvwxyz"

functions = ["sin", "cos", "tg", "exp", "ln"]

readNumber :: String -> Either String (Double, String)
readNumber s =
  if beginsWithZero
    then Left "Numbers are not allowed to begin 0"
    -- throw when invalid dot placement (start/end)
    -- ^ we don't treat such expression as valid, so this meets the requirements
    else Right (read n :: Double, drop (length n) s)
  where
    n = takeWhile (`elem` '.' : digits) s
    beginsWithZero = head n == '0' && length n > 1

startsWith :: String -> String -> Bool
startsWith _ "" = True
startsWith "" _ = False
startsWith (s:str') (w:with')
  | s == w = startsWith str' with'
  | otherwise = False

readFunction :: String -> Either String (String, String)
readFunction s =
  if null funs
    then Left $ "Could not match function '" ++ takeWhile (`elem` characters) s ++ "'"
    else Right (fun, rest)
  where
    funs = filter (\x -> s `startsWith` x) functions -- will match at most one
    fun = head funs
    rest = drop (length fun) s

data Token where
  TOp :: Char -> Token
  TVar :: Char -> Token
  TNum :: Double -> Token
  TFun :: String -> Token
  TLParen :: Token
  TRParen :: Token
  deriving (Eq)

instance Show Token where
  show (TOp c) = show c
  show (TVar c) = show c
  show (TNum n) = show n
  show (TFun fun) = show fun
  show TLParen = show '('
  show TRParen = show ')'

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c : cs)
  | c `elem` operators = (TOp c :) <$> tokenize cs
  | c `elem` variables = (TVar c :) <$> tokenize cs
  | c `elem` digits =
      case readNumber (c : cs) of
        Right (num, rest) -> (TNum num :) <$> tokenize rest
        Left err -> Left err
  | c == '(' = (TLParen :) <$> tokenize cs
  | c == ')' = (TRParen :) <$> tokenize cs
  | otherwise =
      case readFunction (c : cs) of
        Right (fun, rest) -> (TFun fun :) <$> tokenize rest
        Left err -> Left err

-- Little explainer:
-- <$> is the infix notation of fmap, so:
--   c `elem` operators = (TOp c :) <$> tokenize cs
-- is same as:
--   c `elem` operators = fmap (TOp c :) (tokenize cs)
-- or in other words:
--   c `elem` operators =
--     case tokenize cs of
--       Right tokens -> Right $ TOp c : tokens
--       Left err -> Left err

--
-- PARSING
--

parseFunction :: String -> [Token] -> Either String (Expr Double, [Token])
parseFunction fun tokens = case parseExpr nextTokens of
  Right (expr, TRParen : rest') -> case fun of
    "sin" -> Right (Sin expr, rest')
    "cos" -> Right (Cos expr, rest')
    "tg" -> Right (Tan expr, rest')
    "exp" -> Right (Exp expr, rest')
    "ln" -> Right (Log expr, rest')
  Right (a, b) -> Left $ "Missing closing parenthesis for function '" ++ fun ++ "'"
  Left err -> Left err
  where
    nextTokens = tail tokens -- make function parens exclusive for that function (don't parse them in parseExpr)

parseFactor :: [Token] -> Either String (Expr Double, [Token])
parseFactor tokens = case tokens of
  (TOp '-' : TNum n : rest) -> Right (Neg $ Const n, rest)
  (TNum n : rest) -> Right (Const n, rest)
  (TVar 'x' : rest) -> Right (Var 'x', rest)
  (TVar 'y' : rest) -> Right (Var 'y', rest)
  (TVar 'z' : rest) -> Right (Var 'z', rest)
  (TFun fun : rest) -> parseFunction fun rest
  (TLParen : rest) ->
    case parseExpr rest of
      Right (expr, TRParen : rest') -> Right (expr, rest')
      Right (_, _) -> Left "Missing closing parenthesis"
      Left err -> Left err
  _ -> Left "Unexpected token in factor"

parsePower :: [Token] -> Either String (Expr Double, [Token])
parsePower tokens = do
  (base, rest) <- parseFactor tokens
  parsePower' base rest

parsePower' :: Expr Double -> [Token] -> Either String (Expr Double, [Token])
parsePower' base tokens = case tokens of
  (TOp '^' : rest)
    | head rest `elem` [TOp op | op <- operators] -> Left "Binary operator '^' cannot be followed by another operator"
    | otherwise -> do
        (exponent, rest') <- parseFactor rest
        parsePower' (Pow base exponent) rest'
  _ -> Right (base, tokens)

parseTerm :: [Token] -> Either String (Expr Double, [Token])
parseTerm tokens = do
  (fact, rest) <- parsePower tokens
  parseTerm' fact rest

parseTerm' :: Expr Double -> [Token] -> Either String (Expr Double, [Token])
parseTerm' expr tokens = case tokens of
  (TOp '*' : rest)
    | head rest `elem` [TOp op | op <- operators] -> Left "Binary operator '*' cannot be followed by another operator"
    | otherwise -> do
        (fact, rest') <- parsePower rest
        parseTerm' (Mul expr fact) rest'
  (TOp '/' : rest)
    | head rest `elem` [TOp op | op <- operators] -> Left "Binary operator '/' cannot be followed by another operator"
    | otherwise -> do
        (fact, rest') <- parsePower rest
        parseTerm' (Div expr fact) rest'
  _ -> Right (expr, tokens)

parseExpr :: [Token] -> Either String (Expr Double, [Token])
parseExpr tokens = do
  (term, rest) <- parseTerm tokens
  parseExpr' term rest

parseExpr' :: Expr Double -> [Token] -> Either String (Expr Double, [Token])
parseExpr' expr tokens = case tokens of
  (TOp '+' : rest)
    | head rest `elem` [TOp op | op <- operators] -> Left "Binary operator '+' cannot be followed by another operator"
    | otherwise -> do
        (term, rest') <- parseTerm rest
        parseExpr' (Add expr term) rest'
  (TOp '-' : rest)
    | head rest `elem` [TOp op | op <- operators] -> Left "Binary operator '-' cannot be followed by another operator"
    | otherwise -> do
        (term, rest') <- parseTerm rest
        parseExpr' (Add expr (Neg term)) rest'
  _ -> Right (expr, tokens)

parse :: String -> Either String (Expr Double)
parse s = case tokenize s of
  Right tokens -> case parseExpr tokens of
    Right (expr, []) -> Right expr
    Right (_, tokens) -> Left $ "Unused tokens: " ++ show tokens
    Left err -> Left err
  Left err -> Left err

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
s (Add (Neg (Const a)) (Const b)) = Const (b - a)
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
