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

// Only use these when you are sure that the expression is indeed of the
// expected form.
unInt :: Expr -> Int
unInt (Int i) = i
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

// Ints and Bools evaluate to themselves
E expr=:(Int i)  _ _ = expr
E expr=:(Bool b) _ _ = expr

// Just evaluate the function body. Assume that all free variables which occur
// in the function body have already been put in the environment.
E (Fun (FI name)) env funs = E (defBody (funs name)) env funs

// Just lookup the value of the identifier. Because we're evaluating function
// arguments eagerly, there is no need to evaluate the value of variables any
// further.  They are already in normal form.
E (Var (VI name)) env _ = env name

// Put all arguments in the environment, then evaluate the function in that new
// environment.
E (Ap function=:(Fun (FI functionName)) actualParameters) env funs = E function newEnv funs
  where
    // Put all actual parameters into the environment using the corresponding
    // name from the function definition.
    newEnv = foldr (uncurry (|->)) env namedParameters
    // Associate each actual parameter with it's corresponding name from the
    // function definition.  Assume that functions are always applied to the
    // correct number of arguments. This means that the list of formal
    // parameters and the list of actual parameters are of the same length.
    namedParameters :: [(Ident, Expr)]
    namedParameters = zip2 (map unVar (defFormalParameters (funs functionName))) evaluatedParameters
    evaluatedParameters = map (\x -> E x env funs) actualParameters

E (Ap (Prim primitive) actualParameters) env funs = evaluatePrimitive primitive actualParameters env funs
E (Infix lhs primitive rhs)              env funs = evaluatePrimitive primitive [lhs,rhs]        env funs

// Everything else evaluates to the Error expression
E _ _ _ = Error


// Makes heavy use of the assumption that all terms are well-typed.
evaluatePrimitive :: Prim [Expr] State Funs -> Expr

evaluatePrimitive IF [condition:then:else:_] env funs = E (if (unBool $ E condition env funs) then else) env funs

evaluatePrimitive +. actualParameters env funs = evalAndFold sum           actualParameters env funs
evaluatePrimitive *. actualParameters env funs = evalAndFold prod          actualParameters env funs
evaluatePrimitive -. actualParameters env funs = evalAndFold (foldr (-) 0) actualParameters env funs

evaluatePrimitive <. [x:y:_] env funs = Bool (valueX < valueY)
  where valueX = unInt $ E x env funs
        valueY = unInt $ E y env funs

// Evaluate, unwrap, modify, wrap
evaluatePrimitive NOT [b:_] env funs = Bool $ not $ unBool $ E b env funs

// Assume that all actualParameters are integers. Evaluate them, unwrap their
// value, fold them with the given function, then wrap them again
evalAndFold fold actualParameters env funs = Int $ fold $ map unInt $ map (\x -> E x env funs) actualParameters


Ds :: [Def] -> Expr
Ds defs = E (Ap (Fun (FI "Start")) []) newEnv funs
  where
    funs = foldl (\env def=:(Def name _ _) -> (name |-> def) env) newEnv defs

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

  , name "E looks up a variable" $ E (Var (VI "x")) (("x" |-> Int 42) newEnv) newEnv === (Int 42)
  , name "E looks up and evaluates the function which returns constant 42" $
      E (Fun (FI "const42")) newEnv (("const42" |-> CONST42) newEnv) === (Int 42)

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
  ]

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
