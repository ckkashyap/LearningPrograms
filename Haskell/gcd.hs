mygcd m n
 | m == n = m
 | m > n = mygcd (m-n) n
 | n > m = mygcd m (n-m)
