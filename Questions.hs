-- https://wiki.haskell.org/99_questions/11_to_20
module Questions where
    import Data.List 

    numeros = [1,2,3,4]
    pol = [1,2,3,4,3,2,1]
    letras = ["a","e","i","o","u"]
    pak = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    eenc = ["aaaa","b","cc","aa","d","eeee"]
    eencM = "aaaabccaadeeee"


    -- Problem 11
    data encData a = Single Char | Multiple Int Char 

    encodeModified [] = []
    encodeModified (x:xs) = [Multiple (length x) (head x)] ++ encodeModified xs 
                        --   | otherwise = [Single head x] ++ encodeModified xs 