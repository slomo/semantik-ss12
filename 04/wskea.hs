import Control.Monad.State
import qualified Control.Exception as E

-- Semantik von Programmiersprachen
-- AB3 Aufgabe 4
-- Jaehwan Ji
-- Simon Lang

-- aus Aufgabe 1 von AB2
data P = Command COM deriving (Show)
data COM = Skip
         | Assign Id T
         | COM :.: COM
         | IfThenElse BT COM COM
         | WhileDo BT COM
         | OutputBT BT
         | OutputT T
         deriving (Show,Eq)
data BT = Bool W
        | Not BT
        | BoolOp T BOP T
        | ReadBT
        deriving (Show,Eq)
data T = Num Z
       | ID Id
       | Op T OP T
       | ReadT
       deriving (Show,Eq)
data OP = Plus
        | Minus
        | Mult
        | Div
        | Mod
        deriving (Show,Eq)
data BOP = Equal
         | LessThan
         | GreaterThan
         | LessOrEqual
         | GreaterOrEqual
         | Unequal
         deriving (Show,Eq)
data KON = KONNum Z
         | KONBool W
         deriving (Show,Eq)

type Id = String
type W = Bool
type Z = Int

-- AB3 Aufgabe 4
data K = KTerm T
       | KBoolTerm BT
       | KCommand COM
       | KId Id
       | KIf | KWhile | KNot | KOutput | KAssign | KPlus | KMinus | KMult
       | KDiv | KMod | KEqual | KLessThan | KGreaterThan | KLessOrEqual
       | KGreaterOrEqual | KUnequal
       deriving (Show,Eq)

--              W       S     K     E      A
type WSKEAState = ([KON], Memory, [K], [KON], [KON])
type Memory = [(Id, Int)]

showWSKEAState :: WSKEAState -> String
showWSKEAState (w, s, k, e, a)
     = show w ++ "\n"
    ++ show s ++ "\n"
    ++ show k ++ "\n"
    ++ show e ++ "\n"
    ++ show a ++ "\n"

addLinebreaks :: String -> String
addLinebreaks xs = addLinebreaks' xs ""
addLinebreaks' "" output = output
addLinebreaks' (':':'.':':':xs) output = addLinebreaks' xs (output++":.:\n")
addLinebreaks' (x:xs) output = addLinebreaks' xs (output++[x])

-- id := num
assign :: Id -> Z -> Memory -> Memory
assign id num memory
    | lookup id memory == Nothing = (id, num):memory
    | otherwise = (id, num):(filter ((/= id).fst) memory)

-- Anfangszustand
anfang :: P -> [KON] -> WSKEAState
anfang (Command c) input = ([], [], [KCommand c], input, [])

delta :: WSKEAState -> Maybe WSKEAState
-- 1.
-- a)
delta (w, s, (KTerm (Num z)):k, e, a) = Just ((KONNum z):w, s, k, e, a)

-- b)
delta (w, s, (KTerm (ID id)):k, e, a) = case lookup id s of
    Just z -> Just $ ((KONNum z):w, s, k, e, a)
    Nothing -> Nothing

-- c) d) +
delta (w, s, (KTerm (Op t1 Plus t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KPlus:k, e, a)

delta ((KONNum n2):(KONNum n1):w, s, KPlus:k, e, a)
    = Just ((KONNum (n1+n2)):w, s, k, e, a)

-- e) -
delta (w, s, (KTerm (Op t1 Minus t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KMinus:k, e, a)

delta ((KONNum n2):(KONNum n1):w, s, KMinus:k, e, a)
    = Just ((KONNum (n1-n2)):w, s, k, e, a)

-- e) *
delta (w, s, (KTerm (Op t1 Mult t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KMult:k, e, a)

delta ((KONNum n2):(KONNum n1):w, s, KMult:k, e, a)
    = Just ((KONNum (n1*n2)):w, s, k, e, a)

-- e) /
delta (w, s, (KTerm (Op t1 Div t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KDiv:k, e, a)

delta ((KONNum n2):(KONNum n1):w, s, KDiv:k, e, a)
    = Just ((KONNum (n1 `div` n2)):w, s, k, e, a)

-- e) mod
delta (w, s, (KTerm (Op t1 Mod t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KMod:k, e, a)

delta ((KONNum n2):(KONNum n1):w, s, KMod:k, e, a)
    = Just ((KONNum (n1 `mod` n2)):w, s, k, e, a)

-- f)
delta (w, s, (KTerm ReadT):k, e1:e, a)
    = Just (e1:w, s, k, e, a)

-- 2.
-- a)
delta (w, s, (KBoolTerm (Bool b)):k, e, a)
    = Just ((KONBool b):w, s, k, e, a)

-- b) fehlt, da wir keine Bool-Werte speichern können
-- c) d) Equal
delta (w, s, (KBoolTerm (BoolOp t1 Equal t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KEqual:k, e, a)

delta ((KONNum t2):(KONNum t1):w, s, KEqual:k, e, a)
    = Just ((KONBool (t1 == t2)):w, s, k, e, a)

-- e) LessThan
delta (w, s, (KBoolTerm (BoolOp t1 LessThan t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KLessThan:k, e, a)

delta ((KONNum t2):(KONNum t1):w, s, KLessThan:k, e, a)
    = Just ((KONBool (t1 < t2)):w, s, k, e, a)

-- e) GreaterThan
delta (w, s, (KBoolTerm (BoolOp t1 GreaterThan t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KGreaterThan:k, e, a)

delta ((KONNum t2):(KONNum t1):w, s, KGreaterThan:k, e, a)
    = Just ((KONBool (t1 > t2)):w, s, k, e, a)

-- e) LessOrEqual
delta (w, s, (KBoolTerm (BoolOp t1 LessOrEqual t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KLessOrEqual:k, e, a)

delta ((KONNum t2):(KONNum t1):w, s, KLessOrEqual:k, e, a)
    = Just ((KONBool (t1 <= t2)):w, s, k, e, a)

-- e) GreaterOrEqual
delta (w, s, (KBoolTerm (BoolOp t1 GreaterOrEqual t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KGreaterOrEqual:k, e, a)

delta ((KONNum t2):(KONNum t1):w, s, KGreaterOrEqual:k, e, a)
    = Just ((KONBool (t1 >= t2)):w, s, k, e, a)

-- e) Unequal
delta (w, s, (KBoolTerm (BoolOp t1 Unequal t2)):k, e, a)
    = Just (w, s, (KTerm t1):(KTerm t2):KUnequal:k, e, a)

delta ((KONNum t2):(KONNum t1):w, s, KUnequal:k, e, a)
    = Just ((KONBool (t1 /= t2)):w, s, k, e, a)

-- f)
delta (w, s, (KBoolTerm ReadBT):k, e1:e, a)
    = Just (e1:w, s, k, e, a)

-- 3.
-- a)
delta (w, s, (KCommand (Assign id t)):k, e, a)
    = Just (w, s, (KTerm t):(KAssign):(KId id):k, e, a)

-- b)
delta ((KONNum z):w, s, KAssign:(KId id):k, e, a)
    = Just (w, assign id z s, k, e, a)

-- c)
delta (w, s, (KCommand (c1 :.: c2)):k, e, a)
    = Just (w, s, (KCommand c1):(KCommand c2):k, e, a)

-- d)
delta (w, s, (KCommand (IfThenElse b c1 c2)):k, e, a)
    = Just (w, s, (KBoolTerm b):KIf:(KCommand c1):(KCommand c2):k, e, a)

-- e)
delta ((KONBool true):w, s, KIf:(KCommand c1):(KCommand c2):k, e, a)
    = Just (w, s, (KCommand c1):k, e, a)

-- f)
delta ((KONBool false):w, s, KIf:(KCommand c1):(KCommand c2):k, e, a)
    = Just (w, s, (KCommand c2):k, e, a)

-- g)
delta (w, s, (KCommand (WhileDo b c)):k, e, a)
    = Just (w, s, (KBoolTerm b):KWhile:(KBoolTerm b):(KCommand c):k, e, a)

-- h)
delta ((KONBool True):w, s, KWhile:(KBoolTerm b):(KCommand c):k, e, a)
    = Just (w, s, (KCommand c):(KBoolTerm b):KWhile:(KBoolTerm b):(KCommand c):k, e, a)

-- i)
delta ((KONBool False):w, s, KWhile:(KBoolTerm b):(KCommand c):k, e, a)
    = Just (w, s, k, e, a)

-- j)
delta (w, s, (KCommand (OutputT t)):k, e, a)
    = Just (w, s, (KTerm t):KOutput:k, e, a)

-- k)
delta ((KONNum z):w, s, KOutput:k, e, a)
    = Just (w, s, k, e, a++[(KONNum z)])

-- j) k) für Bool
delta (w, s, (KCommand (OutputBT bt)):k, e, a)
    = Just (w, s, (KBoolTerm bt):KOutput:k, e, a)

delta ((KONBool b):w, s, KOutput:k, e, a)
    = Just (w, s, k, e, a++[(KONBool b)])

delta _ = Nothing

-- divprog
divprog :: P
divprog = Command
          (((Assign "x" (ReadT)):.:
            (Assign "y" (ReadT))):.:
           ((Assign "g" (Num 0)):.:                            -- g := 0;
            (WhileDo (BoolOp (ID "x") GreaterOrEqual (ID "y")) -- while (x >= y)
                (Assign "x" (Op (ID "x") Minus (ID "y")):.:    --     x = x - y;
                 Assign "g" (Op (ID "g") Plus (Num 1))))):.:   --     g = g + 1;
            (OutputT (ID "g")))                                -- output g

-- Übung 4, Aufgabe 1
-- we assume existance of a delta and start function
o :: P -> [KON] -> [KON]
o code input = evalState o' (anfang code input)

o' :: State WSKEAState [KON]
o' = do
	oldState <- get
	let maybeNewState = delta oldState
	case maybeNewState of
		Just someState ->
			if someState == oldState
			then return $ output oldState
			else do
				put someState
				o'
		Nothing	-> return []

output (w, s, k, e, a) = a
values (w, s, k, e, a) = w


main :: IO ()
main =
    putStr $ show (o divprog [KONNum 1, KONNum 2])
