fun fib(n) =
    if n = 0 then 1
    else n * fib(n -1)

val _ = print (Int.toString (fib 4))
