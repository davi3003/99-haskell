-- https://wiki.haskell.org/99_questions/1_to_10
module Questions where

    numeros = [1,2,3,4]
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
    
    -- Problem 4
