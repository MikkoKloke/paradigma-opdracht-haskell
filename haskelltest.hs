import Distribution.Simple.Test (test)
import GHC.TypeError (ErrorMessage(Text))
import Distribution.Compat.CharParsing (CharParsing(text))

import Data.Text(pack, unpack, replace)
import Data.Char(isLetter)
import GHC.RTS.Flags (GCFlags(initialStkSize))





jsonSpatieTest = "{\n Person : {\n name:                       \"name of person\",\n height:         1.80,\n weight : \"light as a feather\"\n} \npackageNr :1724627 \n}"

main :: IO ()
main = do
    putStrLn "Json string before --------------------------------------------"
    putStrLn jsonSpatieTest
    putStrLn "Json string after --------------------------------------------"
    putStrLn (unpack (replaceWhitespace (pack jsonSpatieTest)))

replaceWhitespace json = do
    replace (pack " ") (pack "") json

doubleMe :: Num a => a -> a
doubleMe x = 
    x * 2

doubleUs x y =
    doubleMe x + doubleMe y

doubleSmallNumber x = 
    if x > 100
        then x
        else x * 2
    

recursie x =
    if x == 0
        then print x
        else do 
            print x
            recursie (x - 1)        


-- Pattern Matching
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number 7!"
lucky x = "Unlucky maen"

-- Guards
compare' a b
    | a > b =   a ++ " is greater than " ++ b ++ "!"
    | a == b =  "They're the same!"
    | a < b =   b ++ " is greater than " ++ a ++ "!"

makeToken json token
    | nextChar == '{' = makeToken (drop 1 json) token
    | isLetter nextChar = take 1 json
    where nextChar = head json

initials :: String -> String -> String
initials firstName lastName =
    [f] ++ "." ++ [l] ++ "."
        where (f:_) = firstName
              (l:_) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- doink :: (Integral a) => a -> a
-- doink a = case aList of [] -> error "No head for empty lists!"
--                         (x:4:_) -> 4
--                         (x:_) -> x
--                         where aList b = [1,2,3,3]

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : Main.map f xs