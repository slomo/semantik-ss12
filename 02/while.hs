-- Übungsblatt 2 - Semantik von Programmiersprachen
-- ================================================
--
-- Jakob Pfender und Yves Müller
import Data.Map
import Data.Char

--Task 1
type Z = Int
type W = Bool

data K = Z Z | W W
    deriving Show

type I = String

data OP =  Plus | Minus | Mul | Div | Mod
    deriving (Show, Eq)

data BOP = Gt | Lt | Geq | Leq | Eq | Neq
    deriving (Show, Eq)


data T = Num Z | Id I | Term T OP T | ReadInt
    deriving Show

data B = Bool W | Not B | Expr T BOP T | ReadBool
    deriving Show

data C = Skip | Stmt I T | Seq C C | Cond B C C | While B C | OutputInt T | OutputBool B
    deriving Show

type P = C

-- Task 2
divProg :: P
divProg =  ( Seq (While
                (Expr (Id "x") Geq (Id "y") )
                (Seq
                    (Stmt "g" (Term (Id "g") Plus (Num 1)))
                    (Stmt "x" (Term (Id "x") Minus (Id "y")))
                ))
        (OutputInt (Id "g")) )

-- Task 3
evalTerm :: Map String Z -> String -> T -> Z
evalTerm mem input (Num z)              = z
evalTerm mem input (Id name)            = findWithDefault (0)  name mem
evalTerm mem input (ReadInt)            = read input :: Z
evalTerm mem input (Term t1 op t2)
    | op == Plus    = left + right
    | op == Minus   = left - right
    | op == Mul     = left * right
    | op == Div     = div left right
    | op == Mod    = mod left right
        where
            partial = evalTerm mem input
            left = partial t1
            right = partial t2

evalBool :: Map String Z -> String -> B -> W
evalBool mem input (Bool w)           = w
evalBool mem input (Not b)            = not $ evalBool mem input b
evalBool mem input (ReadBool)         = read input :: W
evalBool mem input (Expr t1 op t2)
    | op == Gt      = left > right
    | op == Lt      = left < right
    | op == Geq     = left >= right
    | op == Leq     = left <= right
    | op == Eq      = left == right
    | op == Neq     = left /= right
        where
            partial = evalTerm mem input
            left = partial t1
            right = partial t2

-- Task 4
translateT :: T -> String
translateT (Num z)           = "PUSH " ++ show z
translateT (Id i)            = "LOAD " ++ i
translateT (ReadInt)         = "READ"
translateT (Term t1 op t2)   = translateT t1 ++ "\n" ++ translateT t2 ++ "\n" ++ opStr
    where
        opStr = Prelude.map toUpper $ show op

translateB :: B -> String
translateB (Bool w)         = "PUSH " ++ show w
translateB (Not b)          = translateB b ++ "\n" ++ "NEGATE"
translateB (ReadBool)       = "READ"
translateB (Expr t1 op t2)  = translateT t1 ++ "\n" ++ translateT t2 ++ "\n" ++ opStr
    where
        opStr = Prelude.map toUpper $ show op

-- Minor testing action
main :: IO()
main = do
    let simpleExample = Term (Num 3) Minus (Term (Num (-3)) Plus (Num 4))
    let result  = evalTerm  (empty :: Map String Z) ""  simpleExample
    let code  = translateT  simpleExample
    putStrLn  $ "The result is " ++ ( show result )
    putStrLn  $ "Some stack intermedate code: \n" ++ code

