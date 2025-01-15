fun fib n = 
    if n = 0 then 0 else
    if n = 1 then 1 else
    fib(n-1) + fib(n-2)

fun factorial n =
    if n = 1 then 1 else
    n * factorial(n-1)

val _ = print ("fibonacci: " ^ Int.toString (fib(8)) ^ "\n" );
val _ = print ("factorial: " ^ Int.toString (factorial(5)) ^ "\n");
