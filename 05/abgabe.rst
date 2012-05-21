Übungszettel 5: Semantik von Programmiersprachen
================================================
Yves Müller und Jakob Pfender

Aufgabe 1
---------

T[[read I]] z = {Fehler,                   falls e = ε oder e = b.e'
                {C[[assign I n]] (s,e',a), falls e = n.e'

B[[read I]] z = {Fehler,                   falls e = ε oder e = n.e'
                {C[[assign I b]] (s,e',a), falls e = b.e'

Aufgabe 2
---------

C[[for I:=T TO N do C]] z = {Fehler,      falls T[[T]]z = Fehler oder T[[N]]z = Fehler
                            {C[[C';C]]z', falls B[[T<N]]z = (wahr,z')
                                          wobei C' = I++
                            {C[[C]]z',    falls B[[T=N]]z = (wahr,z')
                            {C[[skip]]z,  falls B[[T<=n]]z = (falsch,z')

Aufgabe 3
---------

B[[eof]] z = {(wahr, z),   falls e = ε
             {(falsch, z), sonst

Aufgabe 4
---------
