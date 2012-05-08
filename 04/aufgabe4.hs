type Wa = Bool
data Lop = And | Or
    deriving (Eq, Show)
data La  = Lit Wa | Expr La Lop La | Not La
    deriving (Eq, Show)


type W = [Wa]
type S = ()
data Ke = G La | SOr | SAnd | SNot
    deriving (Eq, Show)
type K = [Ke]
type E = ()
type A = E


decode :: Lop -> Ke
decode And = SAnd
decode Or = SOr

d :: (W,S,K,E,A) -> (W,S,K,E,A)
d (w, s, (G (Expr b1 op b2)):k, e, a)   = (w, s, (G b1):(G b2):(decode op):k, e, a)
d (w2:w1:w, s, (SAnd):k, e, a)      = ((w1 && w2):w, s, k, e, a)
d (w2:w1:w, s, (SOr):k, e, a)       = ((w1 || w2):w, s, k, e, a)
d (w1:w, s, (SNot):k, e, a)         = ((not w1):w, s, k, e, a)
d (w, s, (G (Lit b)):k, e, a)       = (b:w, s, k, e, a)


r :: ( La, (S,E,A)) -> ( La, (S,E,A))
r ((Not la), (s,e,a))                           = ((Lit (not w)), (s,e,a))
    where
        (Lit w, (_,_,_)) =  r ( la, (s,e,a))
r ((Lit w), (s,e,a))                           = (Lit w, (s,e,a))
r ((Expr (Lit w1) Or (Lit w2)), (s,e,a))        = (Lit (w1 || w2), (s,e,a))
r ((Expr (Lit w1) And (Lit w2)), (s,e,a))       = (Lit (w1 && w2), (s,e,a))
r ((Expr (Lit w1) lop la2), (s,e,a))            = (Expr (Lit w1) lop (Lit w2), (s,e,a))
    where
        (Lit w2, (_,_,_)) =  r ( la2, (s,e,a))
r ((Expr la1 lop la2), (s,e,a))                 = (Expr (Lit w) lop la2, (s,e,a))
    where
        (Lit w, (_,_,_)) =  r ( la1, (s,e,a))
