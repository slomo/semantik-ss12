Semantik von Programmiersprachen
================================

Jakob Pfender und Yves Müller

Aufgabe 1
---------

Wir nehmen an, es gibt eine delta sowie eine Anfangsfunktion.

::

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
            Nothing -> return []

    output (w, s, k, e, a) = a 
    values (w, s, k, e, a) = w 


    main :: IO ()
    main =
        putStr $ show (o divprog [KONNum 1, KONNum 2])

Aufgabe 2
---------

Die Datenstrukturen für die Grammatik haben wir den letzten Zettel entnommen.

::

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
            Just ( Term b, (_, newE, _)) = reduk ( Term bt1, (s, e, a) )
    -- b BOP bt => b BOP c
    reduk ( BTerm (Expr b bop bt), (s, e, a)) = Just ( BTerm (Expr b bop c), (s, newE, a))
        where
            Just ( Term c, (_, newE, _)) = reduk ( Term bt, (s, e, a) )
    -- b > c => (b>c)
    reduk ( BTerm (Expr (Num b) Gt (Num c)), (s, e, a)) = Just ( BTerm ( Bool (b>c)), (s, e, a))
    -- b < c => (b<c)
    reduk ( BTerm ( Expr (Num b) Lt (Num c)), (s, e, a)) = Just ( BTerm ( Bool (b<c)), (s, e, a))
    -- b >= c => (b>=c)
    reduk ( BTerm ( Expr (Num b) Geq (Num c)), (s, e, a)) = Just ( BTerm ( Bool (b>=c)), (s, e, a))
    -- b <= c => (b<=c)
    reduk ( BTerm ( Expr (Num b) Leq (Num c)), (s, e, a)) = Just ( BTerm ( Bool (b<=c)), (s, e, a))
    -- b == c => (b==c)
    reduk ( BTerm ( Expr (Num b) Eq (Num c)), (s, e, a)) = Just ( BTerm ( Bool (b==c)), (s, e, a))
    -- b != c => (b!=c)
    reduk ( BTerm ( Expr (Num b) Neq (Num c)), (s, e, a)) = Just ( BTerm ( Bool (b/=c)), (s, e, a))
    -- read => n
    reduk ( BTerm (ReadBool), (s, (W b):e, a)) = Just (BTerm (Bool b), (s, e, a))
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
            Just ( BTerm (Bool b), (_, newE, _)) = reduk ( BTerm bt, ( s, e, a) )

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

Aufgabe 3
---------

Operationelle Semantik
......................

::

    d(w, s, (LA1 LOP LA2).k, e, a)  = (w, s, LA1.LA2.LOP.k, e, a)       (a)
    d(w2.w1.w, s, AND.k, e, a)      = ((w1 && w2).w, s, k, e, a)        (b)
    d(w2.w1.w, s, OR.k, e, a)       = ((w1 || w2).w, s, k, e, a)        (c)
    d(w, s, (Not LA).k, e, a)       = (w, s, LA1.not.k, e, a)           (d)
    d(w1.w, s, not.k, e, a)         = ((! w1).w, s, k, e, a)            (e)
    d(w, s, true.k, e, a)           = (true.w, s, k, e, a)              (f)
    d(w, s, false.k, e, a)          = (false.w, s, k, e, a)             (g)

Reduktionssemantik
..................

::

    r( LA1 LOP LA2 ,(s,e,a) )       = ( w LOP LA2, (s, e, a))           (1)
        if ( LA1, (s,e,a)) -> ( w, (s,e,a))
    r( w1 LOP LA ,(s,e,a))          = ( w1 LOP w2, (s, e, a))           (2)
        if ( LA, (s,e,a)) -> ( w2, (s,e,a))
    r( w1 AND w2 ,(s,e,a))          = ( w1 && w2, (s, e, a))            (3)
    r( w1 OR w2 ,(s,e,a))           = ( w1 || w2, (s, e, a))            (4)
    r( NOT LA , (s,e,a))            = ( ! w, (s, e, a))                 (5)
        if ( LA, (s,e,a)) -> ( w, (s,e,a))

Equivalenz
..........

Wir beweisen die Equivalenz über die Länge der Ausführung.


Aufgabe 4
---------

::

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

