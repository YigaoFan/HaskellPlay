gcd a b = if (a == b) a if (a < b) (gcd b a) (gcd b (a - b));
main = gcd 6 10;