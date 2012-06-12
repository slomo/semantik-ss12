Semantik von Programmiersprachen 08
===================================

Jakob Pfender und Yves Müller

Aufgabe 1
---------

a)
..

Wir nehmen die cpo der Vergleichsoperation über die natürlichen Zahlen kleiner
100 und bilden sie auf sich selbst ab. Dazu benutzen wir folgende Funktion:

::

    f (x) = { 2         , für x == 100
            { x         , sonst


b)
..

Beweis durch Widerspruch

Wir nehmen an das es zwei Funktionen f ung g gibt, f von A nach B und g von B
nach C. Dann muss es eine Kimposition h = g ° f, die nicht stetig ist. Es gibt
eine Kette K in A mit kleinster oberen Schranke s und h(s) ist nicht kleinste
obere Schranke von h(K) in C.

Da f stetig ist, gibt es ein f(s) in B, dass kleinste obere Schranke der Kette
f(K) ist. Da dies in C nicht gilt, kann g nicht stetig sein. -> Widerspruch

Oder es gibt eine Kette J in C mit kleinster obere Schranke t, aber keine Kette
h-1(J), für die h-1(t) kleinste obere Schranke ist. In diesem Fall war das
Urbild f-1(t) auch in B kleinste obere Schranke der Kette f-1(J). Dann kann aber
f nicht stetig sein. -> Widerspruch

Aufgabe 2
=========


a)
..

Das ist die disjunkte Vereinigung über Mengen, wie sie mathematisch defniert
ist.

b)
..

::
    in_i(d)     = d , falls für allej von 1 bis n und i!=j: d nicht in Dj
    out_i(d)    = d
    is(d)       = true


Aufgabe 3
=========

Für N⊥

::

    add (a,b) = {   b   , wenn a = ⊥
                {   a   , wenn b = ⊥
                { a + b , sonst

    equal (a,b) = { true    , a = ⊥ und b = ⊥
                  { false   , (a = ⊥ und b != ∞) oder (a != ⊥ und b = ∞)
                  { a = b   , sonst

Für Bool⊥

::
    add (a,b) = {   a       , wenn a = ⊥
                {   b       , wenn b = ⊥
                {   true    , wenn a = true oder b = true
                {   false   , sonst

    equal (a,b) = ⊥


Aufgabe 4
=========


