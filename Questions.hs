-- https://wiki.haskell.org/99_questions/11_to_20
module Questions where
    import Data.List 

    numeros = [1,2,3,4]
    pol = [1,2,3,4,3,2,1]
    letras = ["a","e","i","o","u"]
    pak = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    eenc = ["aaaa","b","cc","aa","d","eeee"]
    eencM = "aaaabccaadeeee"
    dec = [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']


    -- Problem 11
    data EncData a = Single a | Multiple Int a deriving Show
    -- encodeModified :: Eq a => [a] -> [EncData a] -- > Not found
    encodeModified [] = []
    encodeModified (x:xs) | length x > 1 = [Multiple (length x) (head x)] ++ encodeModified xs 
                          | otherwise = [Single (head x)] ++ encodeModified xs 

    -- Problem 12
    toTuple :: EncData a -> (Int, a)
    toTuple (Single x)     = (1, x)
    toTuple (Multiple n x) = (n, x)

    decodeModified :: [EncData a] -> [a]
    decodeModified = concatMap (uncurry replicate . toTuple)
    -- decodeModified ((Multiple y x):xs) = [concat(replicate y x)] ++ decodeModified xs
    -- decodeModified ((Single x):xs) = [x] ++ decodeModified xs

    -- Problem 13
        