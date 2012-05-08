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
reduk ( Command (Assign i t), ( s, e, a)) = Just ( Command Skip, ( newS, newE, a))
    where
        Just ( Term (Num z), (_, newE, _)) = reduk ( Term t, ( s, e, a) )
        newS = insert i z s

reduk ( Command (OutputInt t), (s, e, a)) = Just ( Command Skip, (s, newE, [Z z]))
    where
        Just ( Term (Num z), (_, newE, _)) = reduk ( Term t, ( s, e, a) )

reduk ( Term (Id i), (s, e, a)) = Just ( Term (Num z) , (s, e, a))
    where
        Just z = Map.lookup i s

reduk ( Command (Seq c1 c2), (s, e, a)) = Just ( Command c2, (newS, newE, newA))
    where
        Just ( Command Skip, ( newS, newE, newA ) ) = reduk ( Command c1, (s, e, a) )

reduk ( Term ( Num i ), (s, e, a)) = Just ( Term (Num i), (s, e, a) )



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
