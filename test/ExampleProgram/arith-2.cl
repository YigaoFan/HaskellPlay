|| use that functional list
length xs = xs length1 0;
length1 x xs = 1 + (length xs);

main = length (cons 3 (cons 3 (cons 3 nil)));