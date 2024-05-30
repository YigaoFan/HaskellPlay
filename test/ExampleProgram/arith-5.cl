fib n = if (n == 0) 1 (1 + fib (n - 1) + fib (n - 2));
main = fib 4;