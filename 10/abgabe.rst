Semantik von Programmiersprachen 10
===================================

Jakob Pfender und Yves Müller

Aufgabe 1
---------

i
..

::
    
    [ A ↦ N ] ↦ A ↦ N

ii
...

::
    
    ( A x B ) ↦ [ A ↦ B ↦ C ] ↦ C

iii
....

::
    
    [ [ A ↦ A ] ↦ B ] ↦ B

Aufgabe 2
---------

Der Typ und die Definiton des *lit* Operators sind, analog zur Definition von foldr:

::

    [ A ↦ B ↦ B ] ↦ (Ax...xA) ↦ B ↦ B


    lit f as b =  as = (a1,a2...an)     -> f a1 ( lit f (a2...an) b),
                  as = (a1,a2)          -> f a2 f a1 b
                  as = (a1)             -> f a1 b

Wir definieren f als:

::
    
    g a b           = a == b || b
    f (x1,...,xn) x = lit g (x1,...,xn) x) == true

Analog wie oben, nur diesmal wie foldl.

Aufgabe 3
---------

::
    
    C [[ repeate C until B ]] = C [[ C ]] ★ B [[B]] ★ cond ( λz. z, C [[ repeate C until B ]] )
