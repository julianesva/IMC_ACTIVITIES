
import Data.Char (isAlpha, isSymbol)
import Data.List (foldl')
import Data.List (elemIndex)
import System.Win32 (xBUTTON1, COORD (yPos))
--PROBLEM 1: GIVEN A LIST AS INPUT, YOU NEED TO DO A FUNCTION THAT RETURNS THIS LIST BUT THIS TIME ONLY WITH THE POSITIVE NUMBERS OF THE LIST.
positives :: (Ord a, Num a) => [a] -> [a]
positives [] = []
positives (x:xs)
    | x > 0 = x : positives xs
    | otherwise = positives xs

--PROBLEM 2: GIVEN A LIST AS INPUT, YOU NEED TO DO A FUNCTION THAT RETURNS THE SUM OF EVERY NUMBER THAT EXIST IN THE LIST OR RETURN 0 IF TH ELIST IS EMPTY.
add_list :: Num a => [a] -> a
add_list [] = 0
add_list (x:xs) = x + add_list xs

--PROBLEM 3: GIVEN A LIST OF A LIST AS INPUT, YOU NEED TO DO A FUNCTION THAT RETURNS EACH LIST OF THE ORIGINAL LIST INVERT.
invertpairs :: [[a]] -> [[a]]
invertpairs [] = []
invertpairs (x:xs) = reverseList (x) : invertpairs xs

reverseList :: [a] -> [a]
reverseList [] = [] --Base Case
reverseList (x:xs) = reverseList xs ++ [x]

--PROBLEM 4: GIVEN A LIST AS INPUT, YOU NEED TO DO A FUNCTION THAT INDICATES IF EVERY ELEMENT OF THE LIST IS A CHARACTER. IF EVERY ELEMENT IS A CHARACTER YOU NEED TO RETURN TRUE, OTHERWISE FALSE.
symbollist :: [Char] -> Bool
symbollist [] = True
symbollist (x:xs)
    |  not (isAlpha x) && isSymbol x = False
    | otherwise = symbollist xs

--PROBLEM 5: GIVEN A LIST AND TWO MORES VALUES AS INPUT, YOU NEED TO DO A FUNCTION THAT REPLACE EVERY TIME THE FIRST INPUT APPEARS IN THE LIST WITH THE SECOND INPUT.
swapper :: Eq t => t -> t -> [t] -> [t]
swapper _ _ [] = []
swapper a b (x:xs)
    | x == a = b : swapper a b xs
    | otherwise = x : swapper a b xs

--PROBLEM 6: GIVEN TWO LISTS AS INPUT, YOU NEED TO DO A FUNCTION THAT RETURN THE DOT PRODUCT OF THIS TWO LISTS GIVEN AS INPUTS.
dotproduct :: Num a => [a] -> [a] -> a
dotproduct [][] = 0
dotproduct (y:ys)(z:zs) = (y * z) + dotproduct ys zs

--PROBLEM 7: GIVEN A LISTS AS INPUT, YOU NEED TO DO A FUNCTION THAT RETURN THE AVERAGE OF THE ELEMENT INSIDE OF THIS LIST OR 0 IN THE CASE THE INPUT IS AN EMPTY LIST.
average :: Fractional a => [a] -> a
average [] = 0
average xs = sum xs / fromIntegral (listLength xs)

listLength :: [a] -> Int
listLength xs = length xs

--PROBLEM 8: GIVEN A LISTS AS INPUT, YOU NEED TO DO A FUNCTION THAT RETURN THE AVERAGE OF THE ELEMENT INSIDE OF THIS LIST OR 0 IN THE CASE THE INPUT IS AN EMPTY LIST.
standesviation :: Floating a => [a] -> a
standesviation [] = 0
standesviation xs = sqrt (sum (map (\x -> (x-m) ^ 2) xs) / fromIntegral (listLength xs))
    where
        m = average xs

--Problem 9: GIVEN A LIST AND A VALUE AS INPUT, YOU NEED TO DO A FUNCTION THAT GIVE YOU A LIST WITH EACH ELEMENT REPLICATE THE NUMBER OF TIMES THAT THE INPUT VALUE INDICATES.
reeplic :: Int -> [a] -> [a]
reeplic _[] = []
reeplic n bs
    | n < 0 = error "ENTER A POSITIVE INTEGER"
    | otherwise = concatMap (replicate n) bs

--Problem 10: GIVEN A LIST AS INPUT, YOU NEED TO DO A FUNCTION THAT GIVE YOU A LIST WITH EACH ELEMENT REPLICATE THE NUMBER OF TIMES THAT INDICATES THEIR POSITION OR INDEX IN THE LIST.
expand :: [a] -> [a]
expand [] = []
expand xs = concatMap (\(i,x) -> replicate (i+1) x) (zip [0..] xs)

--PROBLEM 11: GIVEN A VALUE GREATHER OR EQUAL TO 0 AS INPUT, YOU NEED TO DO A FUNCTON THAT IF THE VALUE IS EQUAL TO 0 RETURNS AN EMPTY LIST. OTHERWISE THIS FUNCTION NEEDS TO RETURN A LIST WITH THE BINARY NUMBER OR REPRESENTATION OF THIS VALUE.

binary :: Integral t => t -> [t]
binary x
    | x < 0 = error "ENTER A POSITIVE OR EQUAL TO ZERO INTEGER"
    | x == 0 = []
    | otherwise = binary(x `div` 2) ++ [x `mod` 2] 




main :: IO ()
main = do
    putStrLn  "-----------------Solution_Problem_1-----------------"
    putStrLn $ "The result of applying the positive function to your list is: " ++ show (positives [1,-2,3,-4,-5,6])
    putStrLn  "-----------------Solution_Problem_2-----------------"
    putStrLn $ "The result of applying the add_list function to your list is: " ++ show (add_list [1,2,3,4,5])
    putStrLn  "-----------------Solution_Problem_3-----------------"
    putStrLn $ "The result of applying the invertpairs function to your list is: " ++ show (invertpairs [["a","1"],["a","2"],["b","1"],["b","2"]])
    putStrLn  "-----------------Solution_Problem_4-----------------"
    --ASK TEACHER ABOUT THIS PROBLEM.
    putStrLn $ "The result of applying the symbol_list function to your list is: " ++ show (symbollist ['a','b','c','d','e'])
    putStrLn  "-----------------Solution_Problem_5-----------------"
    putStrLn $ "The result of applying the swapper function to your list is: " ++ show (swapper 1 2 [5,4,6,7,1,8,1,9])
    putStrLn  "-----------------Solution_Problem_6-----------------"
    putStrLn $ "The result of applying the dotproduct function to your list is: " ++ show (dotproduct [1,2,3] [4,5,6])
    putStrLn  "-----------------Solution_Problem_7-----------------"
    putStrLn $ "The result of applying the average function to your list is: " ++ show (average [5,6,1,6,0,1,2])
    putStrLn  "-----------------Solution_Problem_8-----------------"
    putStrLn $ "The result of applying the standesviation function to your list is: " ++ show (standesviation [4,8,15,16,23,42])
    putStrLn  "-----------------Solution_Problem_9-----------------"
    putStrLn $ "The result of applying the reeplic function to your list is: " ++ show (reeplic 3 [1,2,3,4])
    putStrLn  "-----------------Solution_Problem_10-----------------"
    putStrLn $ "The result of applying the expand function to your list is: " ++ show (expand [1,2,3,4])
    putStrLn  "-----------------Solution_Problem_11-----------------"
    putStrLn $ "The result of applying the binary function to your number is: " ++ show (binary 30)
    putStrLn  "-----------------Solution_Problem_11-----------------"
