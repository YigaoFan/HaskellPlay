cons a b cc cn = cc a b;
nil cc cn = cn;
head list = list left abort;
tail list = list right abort;
abort = abort;

infinite x = cons x (infinite x);

main = head (tail (infinite 4));