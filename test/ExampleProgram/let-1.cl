oct g x = let h = twice g
  in let k = twice h
  in k (k x);
main = oct id 4;