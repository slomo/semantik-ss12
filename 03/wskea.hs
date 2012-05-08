import Data.Map as Map
-- Definition of while

data Konst = Z Int | W Bool
    deriving Show

type I = String

data OP =  Plus | Minus | Mul | Div | Mod
    deriving (Show, Eq)

data BOP = Gt | Lt | Geq | Leq | Eq | Neq
    deriving (Show, Eq)


data T = Num Int | Id I | Term T OP T | ReadInt
    deriving Show

data B = Bool Bool | Not B | Expr T BOP T | ReadBool
    deriving Show

data C = Skip | Assign I T | Seq C C | If B C C | While B C | OutputInt T | OutputBool B
    deriving Show

type P = C

-- definition of wskea machine
data WE = ZW Bool | WW Int | I I
    deriving Show
data KE = GProg P | GTerm T | GBTerm B | Op OP | Bop BOP | AssignK | WhileK | IfK | NotK | OutputK
    deriving Show

type W = [WE]
type S = Map.Map String Int
type K = [KE]
type E = [Konst]
type A = [Konst]

data State = State  { w :: W
                    , s :: S
                    , k :: K
                    , e :: E
                    , a :: A }


start :: P -> E  -> State
start p e = State { w = []
                , s = Map.empty :: Map String Int
                , k = [GProg p]
                , e = e
                , a = [] }
