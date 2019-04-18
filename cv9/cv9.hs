fact 1 = 1
fact n = n * fact(n-1)

{-
Map - apply fact function on list
-}
--line comment
main = print(map fact [1..10])

--power function
--in:
-- 2^3
--out:
-- 8
power n 0 = 1
power n k = n * power n (k-1)

--this is good on list
--this func returns first two elements in list
doubleHead (x:y:_) = (x,y)

--fibonacci func
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

--tricky
mainy = do
    print(fibonacci 0)
    print(fibonacci 1)
    print(fibonacci 2)
    print(fibonacci 3)

--abs
my_abs n
    | n < 0 = -n
    | otherwise = n
    
--testing
secMain = do
    print(map my_abs [1,2,3,4,-5,-4,-3,-2,-1])
    
--equation
myEq a b = c + (b * a)
    where c = (b * b)
    
--second option on equation with let..i wont do that

--quadr
--a b c 2 5 2 = -2 -0.5
--doesnt work...fuck 
quadr :: Floating a => a -> a -> a -> (a,a) 
quadr a b c = (x1, x2)
    where x1 = (-1*b+sqrt(b*b - 4*a*c))/(2*a)
          x2 = (-1*b-sqrt(b*b - 4*a*c))/(2*a)
          
--prednaska 9
--carrying
add' :: Int -> (Int -> Int)
add' x y  = x+y
--inc = add' 1
-- in 5
--vrati 6
--bere int int a vraci int posledni je vzdy to co vraci
--ord => it is for comparng

-- != nerovna se je v haskellu /=
--list comperhesion
--[x | x <- [1..3]]

--cv9
--
--Write a one line function for a list of all Fibonacci numbers
fibs :: [Int]
fibs = 1:1:[a+b | (a,b) <- zip fibs (tail fibs) ]

--Write a two-line function that lists all prime numbers based on the sieve of Eratosthenes
primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p:sieve [x | x<-xs,  x `mod` p /= 0]


--parametric types
data Answer = Yes | No | Unknown deriving Show

answers :: [Answer]
answers = [Yes, No, Unknown]

--make an own type for representing colours
--define predicate isred determining if colour is red
data Color = Red | Green | Blue deriving (Show, Eq)

isred :: Color -> Bool
isred Red = True
isred _ = False

--make your own type for representing lists of arbitrary elements
-- implements list2mylist
--          length for MyList
data MyList a = Nil | Cons a (MyList a) deriving (Show, Eq)
list2my :: [a] -> MyList a
list2my [] = Nil
list2my (x:xs) = Cons x (list2my xs)

len :: MyList a -> Int
len Nil = 0
len (Cons x xs) = 1 + len xs
