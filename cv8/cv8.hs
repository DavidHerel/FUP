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
quadr a b c = (x1, x2)
    where x1 = (-1*b+sqrt(b*b - 4*a*c))/(2*a)
          x2 = (-1*b-sqrt(b*b - 4*a*c))/(2*a)
          
--split list into 2
--[1,2,3,4,5] -> [[1,3,5],[2,4]]
splitLi li = (li1, li2)
    where li1 = 
          li2 = 

