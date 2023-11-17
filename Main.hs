import Data.Char

isValidISBN :: String -> Bool
isValidISBN isbn = case processISBN isbn of
    Just sumResult -> sumResult `mod` 11 == 0
    Nothing        -> False

processISBN :: String -> Maybe Int
processISBN isbn = do
    let cleanedISBN = map (\c -> if c == 'X' then '0' else c) $ filter (\c -> isDigit c || c == 'X') isbn
    let digits = map digitToInt cleanedISBN
    if length digits /= 10
        then Nothing
        else Just $ sum $ zipWith (*) digits [10, 9..1]

showResult :: Bool -> String
showResult True  = "True"
showResult False = "False"

main :: IO ()
main = do
    putStrLn "Ingrese el código ISBN:"
    userInput <- getLine
    let result = isValidISBN userInput
    putStrLn $ showResult result

