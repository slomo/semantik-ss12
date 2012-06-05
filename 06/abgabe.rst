Übungszettel 6: Semantik von Programmiersprachen
================================================
Yves Müller und Jakob Pfender

Aufgabe 1
---------

::

    { true } x = 7; y = x + 3 { y = 10 }

    { true } x = 7; { x = y - 3  && y = 10 }

    { true } x = 7; { x = 10 - 3 }

    { true } { 7 = 10 - 3 } => { true }

Aufgabe 2
---------

Leider zeitlich nicht geschafft.

Aufgabe 3
---------

::

   {!eof && output.(0+π(input)) = [0+π(input)]}
   {=> output = ε}
   sum := 0;
   {!eof && output.(sum+π(input)) = [sum+π(input)]}
   read x;
   while not (x=0) do
        {!eof && output.(sum+x) = [sum+x]}
        sum := sum + x;
        {!eof && output.sum = [sum]}
        read x;
   {output.sum = [sum] && x=0}
   output sum
   {output = [sum] && x=0}
