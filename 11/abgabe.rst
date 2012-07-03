Semantik von Programmiersprachen 11
===================================

Jakob Pfender und Yves Müller

Aufgabe 1
---------

Das Programm:

::
    
    y := read;  # C1
    x := read;  # C2
    output x/y # C3



Beweis:

::
    
    P [C] <3,2>                 = (C [C] (λz.z)★π) <s0,<3,2>, ε>
    f                           = (λz.z)★π
    z0                          = <s0,<3,2>, ε>
    C [C1;C2;C3] f <3,2> z0     = C[C1] ° C[C2,C3] f <3,2>
    C [x := read] c <3,2> z0    = T[read] λn(s,e,a).c<s[n/I],e,a>
    n0                          = λn(s,e,a).c<s[n/I],e,a>
    T [read] n0 <3,2> z0        = n0(3) <s0, <2>, ε>
    s1 name                     = { 3         , wenn name == x
                                  { s0 name   , sonst
    n0 3 z0                     = c <s1, <2>, ε>

analog für das zweite read

::
    
    C [output x/y] id  s2               = T[x/y] λn(s,e,a).id(s,e,a.n) s2
    g                                   = λn(s,e,a).id(s,e,a.n)
    T [x/y]        g  s2                = T[x] (λn1.T[y] λn2.k(n1 / n2))
    T [x] (λn1.T[y] λn2.k(n1 + n2)) s2  = (λn1.T[y] λn2.g(n1 / n2)) 3
                                        = T[y]  λn2.g(3 / n2)
    T [y]                               = λn2.g(3 / n2) 2
                                        = g(3 /2) = id <s2,ε,<1>>

Aufgabe 2
---------

::
    
    C [[ FOR c1,b1,c2 DO c3 ]]          = C [[ C1 ]] ° C [[ WHILE b1 DO c3;c2 ]]


Aufgabe 3
---------

Wir können jetzt bei der Verkettung von Funktionen abbrechen, in dem wir nicht
mehr die Fortsetzung aufrufen, sondern einfach den Fehler zurück geben.

Aufgabe 4
---------

Unserer Meinung nach ist das equivalent.
