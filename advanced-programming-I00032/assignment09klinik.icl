module assignment09klinik

import StdEnv, gast
/*
  Pieter Koopman, pieter@cs.ru.nl
  2010, 2012
  Skeleton for exercise 9 of the course advanced programming:
  The operational semantics of a strict first order functional programming language
*/

/********************** data types **********************/

:: Var = VI Ident
:: Fun = FI Ident

:: Ident :== String

:: Expr
  = Int   Int
  | Bool  Bool
  | Fun   Fun
  | Var   Var
  | Ap    Expr [Expr]
  | Infix Expr Prim Expr
  | Prim  Prim
  | Error

// Only use this when you are sure that the expression is indeed of the
// expected form.
unBool :: Expr -> Bool
unBool (Bool b) = b

:: Prim = IF | +. | *. | -. | <. | NOT

:: Def = Def Ident [Var] Expr

defBody (Def _ _ expr) = expr
defFormalParameters (Def _ params _) = params

unVar :: Var -> Ident
unVar (VI name) = name

/********************** environments **********************/

:: State :== Env Expr
:: Funs  :== Env Def
:: Env e :== Ident -> e

(|->) infix 9 :: Ident v -> (Env v) -> (Env v)
(|->) v e = \env x.if (x==v) e (env x)

newEnv :: Env e
newEnv = \v.abort ("No binding for " + v)

/********************** semantic functions **********************/

E :: Expr State Funs -> Expr

// Ints, Bools and functions are already in normal form
E expr=:(Int  _) _ _ = expr
E expr=:(Bool _) _ _ = expr
E expr=:(Fun  _) _ _ = expr

// Just lookup the value of the identifier. Because we're evaluating function
// arguments eagerly, there is no need to evaluate the value of variables any
// further.  They are already in normal form.
E (Var (VI name)) env _ = env name

// Put all arguments in the environment, then evaluate the function in that new
// environment.
E (Ap (Fun (FI functionName)) actualParameters) env funs = E functionBody newEnv funs
  where
    (Def _ formalParameters functionBody) = funs functionName
    // Put all actual parameters into the environment using the corresponding
    // name from the function definition.
    newEnv = foldr (uncurry (|->)) env namedParameters
    // Associate each actual parameter with it's corresponding name from the
    // function definition.  Assume that functions are always applied to the
    // correct number of arguments. This means that the list of formal
    // parameters and the list of actual parameters are of the same length.
    namedParameters :: [(Ident, Expr)]
    namedParameters = zip2 (map unVar formalParameters) evaluatedParameters
    evaluatedParameters = map (\x -> E x env funs) actualParameters

E (Ap (Prim primitive) actualParameters) env funs =
    evaluatePrimitive primitive (map (\x -> E x env funs) actualParameters)
E (Infix lhs primitive rhs)              env funs =
    evaluatePrimitive primitive (map (\x -> E x env funs) [lhs,rhs])

// Everything else evaluates to the Error expression
E _ _ _ = Error

instance + Expr where
  (+) (Int x) (Int y) = Int (x + y)

instance * Expr where
  (*) (Int x) (Int y) = Int (x * y)

instance - Expr where
  (-) (Int x) (Int y) = Int (x - y)

instance < Expr where
  (<) (Int x) (Int y) = x < y

instance zero Expr where
  zero = (Int 0)

instance one Expr where
  one = (Int 1)

// The IF case makes use of Clean's lazy evaluation
evaluatePrimitive IF [condition:then:else:_] = if (unBool condition) then else
evaluatePrimitive +. actualParameters = sum actualParameters
evaluatePrimitive *. actualParameters = prod actualParameters
evaluatePrimitive -. actualParameters = foldr (-) (Int 0) actualParameters
evaluatePrimitive <. [x:y:_] = Bool (x < y)
evaluatePrimitive NOT [b:_] = Bool $ not $ unBool $ b

Ds :: [Def] -> Expr
Ds defs = E (Ap (Fun (FI "Start")) []) newEnv funs
  where
    funs = foldl (\env def=:(Def name _ _) -> (name |-> def) env) newEnv defs


/* === expressions of type Int === */

:: IntegerExpression
  = CIConst Int
  | CIVar Ident
  | CIAdd IntegerExpression IntegerExpression
  | CISub IntegerExpression IntegerExpression
  | CIMul IntegerExpression IntegerExpression
  | CIIf BoolExpression IntegerExpression IntegerExpression

derive ggen IntegerExpression
derive genShow IntegerExpression

// converts a IntegerExpression to the corresponding expression in our programming language
intExprToExpr :: IntegerExpression -> Expr
intExprToExpr (CIConst i) = Int i
intExprToExpr (CIVar name) = Var (VI ("i" +++ name)) // prefix name with "i" to avoid name clashes with boolean variables
intExprToExpr (CIAdd lhs rhs) = Infix (intExprToExpr lhs) +. (intExprToExpr rhs)
intExprToExpr (CISub lhs rhs) = Infix (intExprToExpr lhs) -. (intExprToExpr rhs)
intExprToExpr (CIMul lhs rhs) = Infix (intExprToExpr lhs) *. (intExprToExpr rhs)
intExprToExpr (CIIf condition then else) = Ap (Prim IF) [c, t, e]
  where
    c = boolExprToExpr condition
    t = intExprToExpr then
    e = intExprToExpr else


/* === expressions of type Bool === */

:: BoolExpression
  = CBConst Bool
  | CBVar Ident
  | CBSmallerThan IntegerExpression IntegerExpression
  | CBNegation BoolExpression
  | CBIf BoolExpression BoolExpression BoolExpression

derive ggen BoolExpression
derive genShow BoolExpression

boolExprToExpr :: BoolExpression -> Expr
boolExprToExpr (CBConst b) = Bool b
boolExprToExpr (CBVar name) = Var (VI ("b" +++ name)) // prefix name with "b" to avoid name clashes with int variables
boolExprToExpr (CBSmallerThan lhs rhs) = Infix (intExprToExpr lhs) <. (intExprToExpr rhs)
boolExprToExpr (CBNegation e) = Ap (Prim NOT) [boolExprToExpr e]
boolExprToExpr (CBIf condition then else) = Ap (Prim IF) [c, t, e]
  where
    c = boolExprToExpr condition
    t = boolExprToExpr then
    e = boolExprToExpr else

// Extracts all free variables from an expression, and generates an environment
// where they have random values assigned.  Type information is stored in the
// first character of the variable name. b means boolean, i means integer.
// We only use it for expressions generated from IntegerExpression and
// BoolExpression, so some cases won't ever match.
makeEnvironment :: Expr State -> State
makeEnvironment (Int   int) env = env
makeEnvironment (Bool  bool) env = env
//makeEnvironment (Fun   fun) env = env
makeEnvironment (Var (VI name)) env = if (name.[0] == 'i') ((name |-> Int 0) env) ((name |-> Bool True) env)
makeEnvironment (Ap    expr params) env = foldr makeEnvironment env params
makeEnvironment (Infix lhs prim rhs) env = foldr makeEnvironment env [lhs, rhs]
//makeEnvironment (Prim  prim) env = env
//makeEnvironment (Error) env = env

/********************** instances of generic functions for gast **********************/

derive gEq    Expr, Prim, Def, Var, Fun
derive genShow  Expr, Prim, Def, Var, Fun
derive ggen   Expr, Prim, Def, Var, Fun // likely to be improved for real tests

derive bimap []
/********************** some unit tests using gast **********************/

Start
 = test
  [ name "extract first element" $ ("a" |-> "a") (("b" |-> "b") newEnv) "b" == "b"
  , name "extract second element" $ ("a" |-> "a") (("b" |-> "b") newEnv) "a" == "a"
  , name "shadow an element" $ ("a" |-> "a") (("a" |-> "b") newEnv) "a" == "a"

  , name "E on Ints is the identity" $ \x -> E (Int x) newEnv newEnv === Int x
  , name "E on Bools is the identity" $ \x -> E (Bool x) newEnv newEnv === Bool x

  , name "E looks up a variable" $ E (Var (VI "x")) (("x" |-> Int 42) newEnv) newEnv === Int 42

  , name "id 7 == 7" $ E (Ap (Fun (FI "id")) [Int 7]) newEnv (("id" |-> ID) newEnv) === Int 7
  , name "(+) 3 5 == 8" $ E (Ap (Prim +.) [Int 3, Int 5]) newEnv newEnv === Int 8
  , name "(*) 3 5 == 15" $ E (Ap (Prim *.) [Int 3, Int 5]) newEnv newEnv === Int 15
  , name "(-) 3 5 == -2" $ E (Ap (Prim -.) [Int 3, Int 5]) newEnv newEnv === Int (-2)
  , name "NOT True == False" $ E (Ap (Prim NOT) [Bool True]) newEnv newEnv === Bool False
  , name "NOT NOT True == True" $ E (Ap (Prim NOT) [(Ap (Prim NOT) [Bool True])]) newEnv newEnv === Bool True
  , name "3 + 5 == 8" $ E (Infix (Int 3) +. (Int 5)) newEnv newEnv === Int 8
  , name "max 3 5 == 5" $ E (Ap (Fun (FI "max")) [Int 3, Int 5]) newEnv (("max" |-> MAX) newEnv) === Int 5
  , name "max 5 3 == 5" $ E (Ap (Fun (FI "max")) [Int 5, Int 3]) newEnv (("max" |-> MAX) newEnv) === Int 5
  , name "max 5 3 == 5 when fac is also defined" $
      E (Ap (Fun (FI "max")) [Int 5, Int 3]) newEnv (("fac" |-> FAC) (("max" |-> MAX) newEnv)) === Int 5
  , name "5 - 1 < 2 == False" $ E (Infix (Infix (Int 5) -. (Int 1)) <. (Int 2)) newEnv newEnv === Bool False
  , name "dec 3 == 2" $ E (Ap (Fun (FI "dec")) [Int 3]) newEnv (("dec" |-> DEC) newEnv) === Int 2
  , name "(+) ((+) 3 5) 5 == 13" $ E (Ap (Prim +.) [Ap (Prim +.) [Int 3, Int 5], Int 5]) newEnv newEnv === Int 13
  , name "dec ((+) 3 5) == 7" $
      E (Ap (Fun (FI "dec")) [Ap (Prim +.) [Int 3, Int 5]]) newEnv (("dec" |-> DEC) newEnv) === Int 7
  , prop $ E (Ap (Fun (FI "count")) [Int 0]) newEnv (("count" |-> COUNT) newEnv) === Int 1
  , prop $ E (Ap (Fun (FI "count")) [Int 1]) newEnv (("count" |-> COUNT) newEnv) === Int 1
  , prop $ E (Ap (Fun (FI "fac")) [Int 1]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 1
  , prop $ E (Ap (Fun (FI "fac")) [Int 2]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 2
  , prop $ E (Ap (Fun (FI "fac")) [Int 3]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 6
  //, prop $ E (Ap (Fun (FI "twice")) [Fun (FI "inc"),Int 0]) newEnv (("inc" |-> INC) (("twice" |-> TWICE) newEnv)) === Int 3 // higher order
  , prop $ Ds [start0  :defs] === Int 42
  , prop $ Ds [start1 1:defs] === Int 1
  , prop $ Ds [start1 2:defs] === Int 2
  , prop $ Ds [start1 3:defs] === Int 6
  , prop $ Ds [start1 4:defs] === Int 24

  , name "equivalence of prefix and infix notation for operators" prefixInfixEquivalence
  , name "evaluation of arithmetic expressions always yields an integer value" evalIntExprYieldsInt
  , name "evaluation of boolean expressions always yields an integer value" evalBoolExprYieldsBool
  ]

prefixInfixEquivalence :: Int Int -> Property
prefixInfixEquivalence x y = ForEach [+., -., *.]
  (\prim ->
    E (Ap (Prim prim) [Int x, Int y]) newEnv newEnv
    ===
    E (Infix (Int x) prim (Int y)) newEnv newEnv
  )

evalIntExprYieldsInt :: IntegerExpression -> Bool
evalIntExprYieldsInt e =
  case E expr (makeEnvironment expr newEnv) newEnv of
    (Int _) = True
    = False
  where
    expr = intExprToExpr e

evalBoolExprYieldsBool :: BoolExpression -> Bool
evalBoolExprYieldsBool e =
  case E expr (makeEnvironment expr newEnv) newEnv of
    (Bool _) = True
    = False
  where
    expr = boolExprToExpr e

start0   = Def "Start" [] (Int 42)
start1 i = Def "Start" [] (Ap (Fun (FI "fac")) [Int i])

defs = [ID, DEC, INC, FAC, MAX, COUNT]


CONST42 = Def "const42" [(VI "x")] (Int 42)
ID  = Def "id" [(VI "x")] (Var (VI "x"))
MAX = Def "max" [(VI "x"),(VI "y")] (Ap (Prim IF) [Ap (Prim <.) [Var (VI "x"),Var (VI "y")],Var (VI "y"),Var (VI "x")])
DEC = Def "dec" [(VI "x")] (Ap (Prim -.) [Var (VI "x"),Int 1])
INC = Def "inc" [(VI "x")] (Infix (Var (VI "x")) +. (Int 1))
COUNT = Def "count" [(VI "x")] (Ap (Prim IF)
                  [Infix (Var (VI "x")) <. (Int 1)
                  ,Int 1
                  ,Ap (Fun (FI "count")) [Infix (Var (VI "x")) -. (Int 1)]
                  ]
              )
FAC = Def "fac" [(VI "x")] (Ap (Prim IF) [Infix (Var (VI "x")) <. (Int 2)
                  ,Int 1
                  ,Ap (Prim *.) [Var (VI "x")
                           ,Ap (Fun (FI "fac")) [Ap (Fun (FI "dec")) [Var (VI "x")]]
                           ]
                  ]
              )
TWICE = Def "twice" [(VI "f"),(VI "x")] (Ap (Var (VI "f")) [Ap (Var (VI "f")) [Var (VI "x")]]) // this is higher order !!

($) infixr 0 :: (a -> b) a -> b
($) f a = f a
