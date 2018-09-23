-- https://wiki.haskell.org/99_questions/1_to_10
module Questions where

    numeros = [1,2,3,4]
    pol = [1,2,3,4,3,2,1]
    letras = ["a","e","i","o","u"]

    -- Problem 1
    myLast :: [a] -> a
    myLast [] = error "empty lists!"
    myLast [x] = x
    myLast (x:xs) = myLast xs
    
    -- Problem 2
    myButLast :: [a] -> a
    myButLast [] = error "empty lists!"
    myButLast [x] = error "list more 2 itens!"
    myButLast (x:y:[]) = x
    myButLast (x:xs) = myButLast xs 

    -- Problem 3
    elementAt :: [a] -> Int -> a 
    elementAt [] _ = error "empty lists!"
    elementAt _ 0 = error "Non zero index"
    elementAt (x:_) 1 = x
    elementAt (x:xs) k | k == 1 = x
                       | otherwise = elementAt xs (k-1)
    -- elementAt (x:xs) k = elementAt xs (k-1)
    
    -- Problem 4
    myLength :: [a] -> Int 
    myLength [] = 0
    myLength (x:xs) = 1 + myLength xs

    -- Problem 5
    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x:xs) = myReverse xs ++ [x]

    -- Problem 6
    -- isPalindrome :: [a] -> Bool
    isPalindrome [] = False
    isPalindrome [x] = True
    isPalindrome (x:y:[]) = False
    isPalindrome (x:xs) | last xs == x = isPalindrome (init xs)
                        | otherwise = False
    -- isPalindrome (x:xs) = if last xs == x then True
    --                       else False