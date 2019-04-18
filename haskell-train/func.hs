--otypovani
double :: Int -> Int
double x = x +x
quad x = double (double x)

fact1 :: Int -> Int
fact1 1 = 1
fact1 n = n * fact1 (n-1)

fact2 n = product [1..n]

power n 0 = 1
power n k = n * power n (k-1)

{-
komentarrr
function starts with small letter...for instance: myFun
list ends with s....xs, ns, nss

-}

{-
operatory maji mensi prednost nez fce
. = spojeni fci
$ = asociace
-}
myFc x = (sqrt . sin) x
myFc2 x = sqrt $ sin x

not1 False = True
not1 True = False

and1 True True = True
and1 _ _ = False

and2 True b = b
and2 False _ = False


my_head (x:_) = x
my_tail (_:xs) = xs

my_len [] = 0
my_len (x:xs) = 1 + my_len xs

last' (x:[]) = x
last' (_:xs) = last' xs

first (x,y) = x
second (x,y) = y

dist (x1, y1) (x2, y2) = 
    let dx = x1 - x2
        dy = y1 - y2
    in sqrt (dx^2 + dy^2)
    
dist2 (x1, y1) (x2, y2) =  sqrt(dx^2 + dy^2)
    where dx = x1 - x2
          dy = y1 - y2
          
          
signum' n | n < 0 = -1
          | n == 0 = 0
          | otherwise = 1
    
{-
[x^2 | x <- [1..5]]
[(x,y) | x <- [1..5], y <- ["a", "b"]]

-}

flatten1 xss= [x | xs<-xss, x<-xs]

factors n = [x | x <- [1..n], mod n x == 0]

prime n = factors n == [1,n]

primes = [x | x <- [1..], prime x]

qsort [] = []
qsort(x:xs) = qsort[a | a <-xs, a < x]++ [x] ++qsort[a | a <-xs, a >= x]

qsort2[]     = []
qsort2(x:xs) = qsort smalls ++ [x] ++ qsort larges 
    where
        smalls = [a | a <-xs, a <=x]
        larges = [b | b <-xs, b > x]
        
--prednaska 9
--carrying
add' :: Int -> (Int -> Int)
add' x y  = x+y
--inc = add' 1
-- in 5
--vrati 6





