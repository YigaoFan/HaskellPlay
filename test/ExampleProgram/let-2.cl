infinite x = letrec xs = cons x xs
  in xs;

main = head (tail (tail (infinite 4)));