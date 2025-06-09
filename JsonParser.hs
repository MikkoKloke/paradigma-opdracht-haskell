import Data.Text (pack, replace, unpack)
import Data.List (isPrefixOf)
import Data.Char (isDigit)
import Distribution.PackageDescription (mapTreeConds)-- import Data.ByteString.Char8 (putStrLn)

----- Lexer Tokens -----
leftBrace = '{'
rightBrace = '}'
leftBracket = '['
rightBracket = ']'
quotation = '\"'
colon = ':'
comma = ','


--- Lexer Token Stream ---
data LexerToken = LeftBraceToken
                | RightBraceToken
                | LeftBracketToken
                | RightBracketToken
                | ColonToken
                | CommaToken
                | TextToken String
                | NumberToken String
                | BooleanToken Bool
                deriving (Show)-- NumberToken INT OR FLOAT?

type TokenStream = [LexerToken]


----- Parser Tree -----
data ParseTree =  JsonObjectNode [(String, ParseTree)]
                | TextNode String
                | NumberNode String
                | ArrayNode [ParseTree]
                | BooleanNode Bool
                -- | NullValue - try later
                deriving Show


rawJsonInput1 = "{\n \"destination\" : \"Japan\", \n \"Person\" : {\n \t\"name\" : true,\n \t\"height\" : 150,\n \t\"weight\" : \"light as a feather\"\n\t} , \n\"packageNr\" :7124627 \n}"
rawJsonInput2 = "{\n\t\"naam\" : \"Marit\",\n\t\"hobbies\" : [\"Tekenen\", \"RDR2\", \"Murder Mystery\"],\n\t\"werk\" : {\n\t\t\"naam\" : \"Starbucks\",\n\t\t\"functie\" : \"Barista\",\n\t\t\"locatie\" : {\n\t\t\t\"naam\" : \"Nijmegen Centraal Station\",\n\t\t\t\"Adres\" : \"6512 AB Nijmegen\",\n\t\t\t\"breedteGraad\" : 51.843742,\n\t\t\t\"lengteGraad\" : 5.8537626,\n\t\t\t\"toegankelijk mindervaliden\" : true\n\t\t}\n\t},\n\t\"lengte\" : 1.60\n}"

-- testPureFunctions = do
--     -- first print a variable
--     printJson

--     -- then set its value to something else. Note: you cannot use the assignment operator without the let, so we are essentially initialising the same variable locally.
--     let rawJsonInput = "iets anders"
--     -- print the new value
--     print rawJsonInput

--     -- but when the same variable is printed outside of the function, the value is the old one.
--     printJson

-- printJson = print rawJsonInput

----- Main Function -----
parseJson :: String -> IO ()
parseJson rawJson = do
    putStrLn " ----- RAWJSON ----- "
    putStrLn rawJson
    putStrLn ""

    let rawJsonInput = 123

    let jsonWithoutWhitespace = removeWhiteSpace rawJson
    putStrLn " ----- JSONWITHOUTWHITESPACE ----- "
    putStrLn jsonWithoutWhitespace
    putStrLn ""

    let tokenStream = lexerGetTokenStream jsonWithoutWhitespace
    putStrLn " ----- TOKENSTREAM ----- "
    print tokenStream
    putStrLn ""

    -- let parseTree = fst (parseNextToken tokenStream)
    let parseTree = parseTokenStream tokenStream
    putStrLn " ----- PARSETREE ----- "
    print parseTree
    putStrLn ""

removeWhiteSpace :: [Char] -> String
removeWhiteSpace string = do
    let text = pack string
    unpack (replace (pack "\n") (pack "") (replace (pack "\t") (pack "") (replace (pack " ") (pack "") text)))


----- Lexer Functions -----
lexerGetTokenStream :: String -> TokenStream
lexerGetTokenStream json = lexerMakeTokenStream [] json

lexerMakeTokenStream :: TokenStream -> String -> TokenStream
lexerMakeTokenStream tokenStream json
    -- Base case for the recursion, if all the json is tokenified give back the tokenStream.
    | null json                 = tokenStream
    -- Simple 1 character tokens.
    | nextChar == leftBrace     = lexerMakeTokenStream (tokenStream ++ [LeftBraceToken]) (tail json)
    | nextChar == rightBrace    = lexerMakeTokenStream (tokenStream ++ [RightBraceToken]) (tail json)
    | nextChar == leftBracket   = lexerMakeTokenStream (tokenStream ++ [LeftBracketToken]) (tail json)
    | nextChar == rightBracket  = lexerMakeTokenStream (tokenStream ++ [RightBracketToken]) (tail json)
    | nextChar == colon         = lexerMakeTokenStream (tokenStream ++ [ColonToken]) (tail json)
    | nextChar == comma         = lexerMakeTokenStream (tokenStream ++ [CommaToken]) (tail json)
    -- If you encounter a quotation, it is an identifier or text value, drop the quotation and make the identifiertoken with the remaining stream
    | nextChar == quotation     = let textToken = lexerMakeTextToken "" (tail json)
                                  in  lexerMakeTokenStream (tokenStream ++ [TextToken textToken]) (drop (length textToken + 2) json) -- Add 2 to the drop amount, to account for the two quotations that need to be removed. 
    -- If you encounter a number, it is a number value
    | lexerIsJsonNumber nextChar     = let numberToken = lexerMakeNumberToken "" json
                                       in  lexerMakeTokenStream (tokenStream ++ [NumberToken numberToken]) (drop (length numberToken) json)
    | "true" `isPrefixOf` json  = lexerMakeTokenStream (tokenStream ++ [BooleanToken True]) (drop 4 json)
    | "false" `isPrefixOf` json = lexerMakeTokenStream (tokenStream ++ [BooleanToken False]) (drop 5 json)
    | otherwise                 = error ("Invalid json, on character: " ++ [nextChar])
    where nextChar = head json

----- Lexer Helper Functions -----
lexerMakeTextToken :: [Char] -> [Char] -> [Char]
lexerMakeTextToken token json
    | nextChar == quotation = token
    | otherwise = lexerMakeTextToken (token ++ [nextChar]) (tail json)
    where nextChar = head json

lexerMakeNumberToken :: [Char] -> [Char] -> [Char]
lexerMakeNumberToken token json
    | lexerIsJsonNumber nextChar || nextChar == '.' = lexerMakeNumberToken (token ++ [nextChar]) (tail json)
    | otherwise = token
    where nextChar = head json

lexerIsJsonNumber :: Char -> Bool
lexerIsJsonNumber character = character == '.' || isDigit character


----- Parser Functions -----
parseTokenStream :: TokenStream -> ParseTree
parseTokenStream tokenStream = parseTree
                                        -- GOES TO PARSENEXTTOKEN TO PARSE THE ROOT JSON OBJECT
    where (parseTree, remainingTokens) = parseNextToken tokenStream

parseNextToken :: TokenStream -> (ParseTree, TokenStream)
parseNextToken [] = (TextNode "null", [])
-- ENTERS HERE FOR THE FIRST TIME FOR THE ROOT JSON OBJECT - LATER COMES BACK TO THIS FUNCTION FOR THE REST OF THE IDENTIFIERS OF THE ROOT OBJECT, WHICH ARE THE BASE JSON VALUES, AND ENTERS THIS LINE ONLY WHEN ENCOUNTERING A SUB-OBJECT.
parseNextToken (LeftBraceToken:rest) = parseObject [] rest
parseNextToken (LeftBracketToken:rest) = parseArray [] rest
parseNextToken (TextToken text:rest) = (TextNode text, rest)
parseNextToken (NumberToken number:rest) = (NumberNode number, rest)
parseNextToken (BooleanToken bool:rest) = (BooleanNode bool, rest)
-- If the next token does not match to any of the above, the syntax is incorrect.
parseNextToken invalidTokens = error ("Syntax Error: Next token in stream is not a valid token. Remainder of stream: " ++ show invalidTokens)


parseObject :: [(String, ParseTree)] -> TokenStream -> (ParseTree, TokenStream)
-- BASE CASE: If the next token is a RightBraceToken, the object ends here and needs to be returned, together with the remaining token stream.
parseObject identifiers (RightBraceToken:remainderOfStream) = (JsonObjectNode identifiers, remainderOfStream)
-- If the next token is a TextToken, followed by a ColonToken, it is an identifier.
parseObject identifiers (TextToken identifierKey:ColonToken:rest) = let (identifierValue, remainingObject) = parseNextToken rest
                                                                                            -- SUB-BASE CASE: If there is a rightbracetoken after the identifier, the object closes
                                                                    in case remainingObject of RightBraceToken:restOfTokenStream -> (JsonObjectNode (identifiers ++ [(identifierKey , identifierValue)]), restOfTokenStream)
                                                                                            -- If there is a CommaToken after the identifier, add the currect identifier to the object and parse the next identifier its remaining tokens.
                                                                                               CommaToken:restOfObject -> parseObject (identifiers ++ [(identifierKey, identifierValue)]) restOfObject
                                                                                               incorrectNextToken -> error ("Syntax Error: Expected ',' or '}' after object-identifier with key: " ++ identifierKey)
parseObject identifiers tokens = error ("Syntax Error: Unexpected tokens in object. \n Remainder of tokenstream: " ++ show tokens)


parseArray :: [ParseTree] -> TokenStream -> (ParseTree, TokenStream)
-- BASE CASE: If the next token is a RightBracketToken, the array ends here and needs to be returned, together with the remaining token stream.
parseArray array (RightBracketToken:remainderOfStream) = (ArrayNode array, remainderOfStream)
parseArray array tokens = let (arrayElementValue, remainingArray) = parseNextToken tokens
                          in case remainingArray of RightBracketToken:restOfTokenStream -> (ArrayNode (array ++ [arrayElementValue]), restOfTokenStream)
                                                    CommaToken:restOfArray -> parseArray (array ++ [arrayElementValue]) restOfArray
                                                    incorrectNextToken -> error "Syntax Error: Expected ',' or ']' in an array"