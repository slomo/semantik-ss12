Operationelle Semantik
----------------------

::
    d(w, s, (LA1 LOP LA2).k, e, a)  = (w, s, LA1.LA2.LOP.k, e, a)       (a)
    d(w2.w1.w, s, AND.k, e, a)      = ((w1 && w2).w, s, k, e, a)        (b)
    d(w2.w1.w, s, OR.k, e, a)       = ((w1 || w2).w, s, k, e, a)        (c)
    d(w, s, (Not LA).k, e, a)       = (w, s, LA1.not.k, e, a)           (d)
    d(w1.w, s, not.k, e, a)         = ((! w1).w, s, k, e, a)            (e)
    d(w, s, true.k, e, a)           = (true.w, s, k, e, a)              (f)
    d(w, s, false.k, e, a)          = (false.w, s, k, e, a)             (g)

Reduktionssemantik
------------------

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
----------

Wir beweisen die Equivalenz über die Länge der Ausführung.


