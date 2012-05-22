Übungszettel 5: Semantik von Programmiersprachen
================================================
Yves Müller und Jakob Pfender

Aufgabe 1
---------

::
    
    C[[read I]] z = {Fehler,                   falls e = ε
                {C[[assign I n]] (s,e',a), falls e = n.e'

Aufgabe 2
---------

::
    
    C[[for I:=T TO N do C]] z = {Fehler,      falls T[[T]]z = Fehler oder T[[N]]z = Fehler
                            {C[[C';C]]z', falls B[[T<N]]z = (wahr,z')
                                          wobei C' = I++
                            {C[[C]]z',    falls B[[T=N]]z = (wahr,z')
                            {C[[skip]]z,  falls B[[T<=n]]z = (falsch,z')

Aufgabe 3
---------

::
    
    B[[eof]] z = {(wahr, z),   falls e = ε
             {(falsch, z), sonst

    B[[read I]] z = {B[[eof]] z,                              falls e = ε
                {C[[assign I n]] ( B[[False]], (s,e',a)), falls e = n.e'

Aufgabe 4
---------

Implementierung der Summe in WHILE:

::
    
    sum := 0;
    WHILE not read input DO
        sum := sum + input

Verfikation mit denotatieller Semantik:

::
    
    C [[ sum := 0; WHILE ... ]] z   = (s0, [1,2], ε) = C [[ WHILE ... ]] ( C [[ sum := 0 ]] z)
    C [[ sum := 0 ]] (s0, [1,2], ε) = (s1, [1,2], ε), wobei s1 x = { 0      , x = sum
                                                                   { s0(x)  , sonst
    C [[ WHILE not read input DO sum := ... ]] z
                                    = C [[ sum :=  ... ; WHILE ... ]] z',
                                        da B [[ not read input ]] z =  z'
                                        wobei z = ( s2, [2], ε) und s2 x = { 2     , x = input
                                                                           { s1(x) , sonst
    C [[ sum := sum + input; WHILE ... ]] z = C [[ WHILE ... ]] (C [[ sum := sum + input ]], z)
    C [[ sum := sum + input ]] (s2, [1], ε) = ( s3, [1], ε), wobei
                                                3 = T [[ sum + input ]] und
                                                s3 x = { 3      , x = sum
                                                       { s2(x)  , sonst
    C [[ WHILE not read input DO sum := ... ]] ( s3, [2], ε) = ...

Verfahre analog für das nächste Eingabeelement

Ohne unsere Definiton von EOF wäre das Programm deulich länger. Komplett ohne EOF wäre die
Implementierung gar unmöglich, da in unserer Sprache keine Fehler gefangen werden können, und
auch keine Überprüfung der verbleibenden Eingabelänge möglich ist.
