import Data.Map as Map
-- Definition of while

data Konst = Z Int | W Bool
    deriving Show

type I = String

data OP =  Plus | Minus | Mul | Div | Mod
    deriving (Show, Eq)

data BOP = Gt | Lt | Geq | Leq | Eq | Neq
    deriving (Show, Eq)


data T = Num Int | Id I | Op T OP T | ReadInt
    deriving Show

data B = Bool Bool | Not B | Expr T BOP T | ReadBool
    deriving Show

data C = Skip | Assign I T | Seq C C | If B C C | While B C | OutputInt T | OutputBool B
    deriving Show

type P = C

-- definition of wskea machine
data Phrase = Command C | Term T | BTerm B
    deriving Show

type E = [Konst]
type A = E
type S = Map String Int

reduk :: ( Phrase, ( S, E, A) ) -> Maybe ( Phrase, ( S, E, A) )
-- x => S(x)
reduk ( Term (Id i), (s, e, a)) = Just ( Term (Num z) , (s, e, a))
    where
        Just z = Map.lookup i s
-- T1 OP T2 => n OP T2
reduk ( Term (Op t1 op t2), (s, e, a)) = Just ( Term (Op n op t2), (s, newE, a))
    where
        Just ( Term n, (_, newE, _)) = reduk ( Term t1, (s, e, a) )
-- n OP T => n OP m
reduk ( Term (Op n op t), (s, e, a)) = Just ( Term (Op n op m), (s, newE, a))
    where
        Just ( Term m, (_, newE, _)) = reduk ( Term t, (s, e, a) )
-- n + m => (n+m)
reduk ( Term (Op (Num n) Plus (Num m)), (s, e, a)) = Just ( Term ( Num (n+m)), (s, e, a))
-- n - m => (n-m)
reduk ( Term (Op (Num n) Minus (Num m)), (s, e, a)) = Just ( Term ( Num (n-m)), (s, e, a))
-- n * m => (n*m)
reduk ( Term (Op (Num n) Mul (Num m)), (s, e, a)) = Just ( Term ( Num (n*m)), (s, e, a))
-- n / m => (n/m)
reduk ( Term (Op (Num n) Div (Num m)), (s, e, a)) = Just ( Term ( Num (div n m)), (s, e, a))
-- n % m => (n%m)
reduk ( Term (Op (Num n) Mod (Num m)), (s, e, a)) = Just ( Term ( Num (mod n m)), (s, e, a))
-- read => n
reduk ( Term (ReadInt), (s, (Z z):e, a)) = Just (Term (Num z), (s, e, a))
-- bt1 BOP bt2 => b BOP bt2
reduk ( BTerm (Expr bt1 bop bt2), (s, e, a)) = Just ( BTerm (Expr b bop bt2), (s, newE, a))
    where
        Just ( BTerm (Bool b), (_, newE, _)) = reduk ( BTerm bt1, (s, e, a) )
-- b BOP bt => b BOP c
reduk ( BTerm (Expr b bop bt), (s, e, a)) = Just ( BTerm (Expr b bop c), (s, newE, a))
    where
        Just ( BTerm (Bool c), (_, newE, _)) = reduk ( BTerm bt, (s, e, a) )
-- b > c => (b>c)
reduk ( BTerm (Gt b c), (s, e, a)) = Just (b>c, (s, e, a))
-- b < c => (b<c)
reduk ( BTerm (Lt b c), (s, e, a)) = Just (b<c, (s, e, a))
-- b >= c => (b>=c)
reduk ( BTerm (Geq b c), (s, e, a)) = Just (b>=c, (s, e, a))
-- b <= c => (b<=c)
reduk ( BTerm (Leq b c), (s, e, a)) = Just (b<=c, (s, e, a))
-- b == c => (b==c)
reduk ( BTerm (Eq b c), (s, e, a)) = Just (b==c, (s, e, a))
-- b != c => (b!=c)
reduk ( BTerm (Neq b c), (s, e, a)) = Just (b/=c, (s, e, a))
-- read => n
reduk ( Term (ReadBool), (s, b:e, a)) = Just (Term (Bool b), (s, e, a))
-- I := T => skip, assign
reduk ( Command (Assign i t), ( s, e, a)) = Just ( Command Skip, ( newS, newE, a))
    where
        Just ( Term (Num z), (_, newE, _)) = reduk ( Term t, ( s, e, a) )
        newS = insert i z s
-- C1; C2 => skip, C2
reduk ( Command (Seq c1 c2), (s, e, a)) = Just ( Command c2, (newS, newE, newA))
    where
        Just ( Command Skip, ( newS, newE, newA ) ) = reduk ( Command c1, (s, e, a) )
-- if B then C1 else C2 => ...
reduk ( Command (If b c1 c2), (s, e, a)) = Just ( Command c1, (newS, newE, newA))
    where
        Just ( BTerm (Bool True), (newS, newE, newA) ) = reduk (BTerm b, (s, e, a))
reduk ( Command (If b c1 c2), (s, e, a)) = Just (Command c2, (newS, newE, newA))
    where
        Just ( BTerm (Bool False), (newS, newE, newA) ) = reduk (BTerm b, (s, e, a))
-- while B do C => ...
reduk ( Command (While b c), (s, e, a)) = Just ( Command Skip, (s, newE, a))
    where
        Just ( BTerm (Bool False), (_, newE, _)) = reduk ( BTerm b, (s, e, a))
reduk ( Command (While b c), (s, e, a)) = Just ( Command (Seq c (While b c)), (s, newE, a))
    where
        Just ( BTerm (Bool True), (_, newE, _)) = reduk ( BTerm b, (s, e, a))
-- output T => skip
reduk ( Command (OutputInt t), (s, e, a)) = Just ( Command Skip, (s, newE, [Z z]))
    where
        Just ( Term (Num z), (_, newE, _)) = reduk ( Term t, ( s, e, a) )
-- output B => skip
reduk ( Command (OutputBool bt), (s, e, a)) = Just ( Command Skip, (s, newE, [W b]))
    where
        Just ( BTerm (Bool b), (_, newE, _)) = reduk ( BTerm b, ( s, e, a) )

reduk ( Term ( Num i ), (s, e, a)) = Just ( Term ( Num i), (s, e, a) )
reduk (BTerm (Bool b ), (s, e, a)) = Just (BTerm (Bool b), (s, e, a) )

eval :: P -> E -> A
eval p e =  a
    where
        Just ( c1, ( s1, e1, a1)) = reduk ( Command p,( empty, e, []) )
        Just ( Command Skip, ( _, _, a)) = reduk ( c1 ,( s1, e1, a1) )

test :: P
test = ( Seq ( Assign "bla" ( Num 2)) (OutputInt (Id "bla")))

main :: IO ()
main = do
    putStr $ show $ eval test []
