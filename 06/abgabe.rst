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

   {INV = {e_1,...,e_n-1 != 0 && e_n = 0 && n <= m && output = ε}}
   {input = (e_1,...,e_m) && e_1,...,e_n-1 != 0 && e_n == 0 && n<= m && output = ε}
   {0 + e_j + sum[e in input] = sum[i=1 to m](e_i) && j <= n && x = e_j && INV && input=(e_j+1,...,e_m)}
   sum := 0;
   {sum + e_j + sum[e in input] = sum[i=1 to m](e_i) && j <= n && x = e_j && INV && input=(e_j+1,...,e_m)}
   read x;
   {sum + x + sum[e in input] = sum[i=1 to m](e_i) && j-1 <= n && x = e_j-1 && INV && input=(e_j,...,e_m)}
   while not (x=0) do
   {x != 0 && sum + x + sum[e in input](e) = sum[i=1 to m](e_i) && input=(e_j,...,e_m) && j-1 <= n && x = e_j-1 && INV}
        {sum + x + e_j + sum[e in input](e) = sum[i=1 to m](e_i) && input=(e_j+1,...,e_m) && j <= n && x = e_j && INV}
        sum := sum + x;
        {sum + e_j + sum[e in input](e) = sum[i=1 to m](e_i) && input=(e_j+1,...,e_m) && j <= n && x = e_j && INV}
        read x;
        {sum + x + sum[e in input](e) = sum[i=1 to m](e_i) && input=(e_j,...,e_m) && j-1 <= n && x = e_j-1 && INV}
   {x=0 && INV && j-1 <= n && x = e_j-1 && sum + x + sum[e in input](e) = sum[i=1 to m](e_i)}
   =>{x = e_n && input=(e_n+1,...,e_m)}
   =>{sum = sum[i=1 to n-1](e_i) && output = ε}
   =>{output.sum = sum[i=1 to n-1](e_i)}
   output sum
   {output = sum[i=1 to n-1](e_i)}
