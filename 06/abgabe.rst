Übungszettel 6: Semantik von Programmiersprachen
================================================
Yves Müller und Jakob Pfender

Aufgabe 1
---------

Aufgabe 2
---------

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
