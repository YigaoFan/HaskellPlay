downfrom n = if (n == 0) nil (cons n (downfrom (n - 1)));
main = downfrom 4;