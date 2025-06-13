import Data.List (isPrefixOf)
import Data.Char (isDigit, isSpace)

----- Lexer -----
leftBrace = '{'
rightBrace = '}'
leftBracket = '['
rightBracket = ']'
quotation = '\"'
escapedQuotation = "\\\""
colon = ':'
comma = ','


--- Lexer Token Stream ---
type TokenStream = [LexerToken]

data LexerToken = LeftBraceToken
                | RightBraceToken
                | LeftBracketToken
                | RightBracketToken
                | ColonToken
                | CommaToken
                | TextToken String
                | NumberToken String
                | BooleanToken Bool
                | NullToken
                deriving (Show)


--- Parser Tree ---
data ParseTree =  JsonObjectNode [(String, ParseTree)]
                | TextNode String
                | NumberNode String
                | ArrayNode [ParseTree]
                | BooleanNode Bool
                | NullNode
                deriving Show

----- Main Function -----
parseJson :: String -> IO ()
parseJson jsonFileName = do
    rawJson <- readFile jsonFileName -- "rawJsonInput.json"

    putStrLn " ----- RAWJSON ----- "
    putStrLn rawJson
    putStrLn ""

    let tokenStream = lexerMakeTokenStream [] rawJson
    putStrLn " ----- TOKENSTREAM ----- "
    print tokenStream
    putStrLn ""

    let parseTree = fst (parseNextToken tokenStream)
    putStrLn " ----- PARSETREE ----- "
    print parseTree
    putStrLn ""


----- Lexer Functions -----
lexerMakeTokenStream :: TokenStream -> String -> TokenStream
lexerMakeTokenStream tokenStream json
    -- Base case for the recursion, if all the json is tokenified give back the tokenStream.
    | null json                 = tokenStream
    -- Ignore whitespace
    | isSpace nextChar          = lexerMakeTokenStream tokenStream (tail json)
    -- Simple 1 character tokens.
    | nextChar == leftBrace     = lexerMakeTokenStream (tokenStream ++ [LeftBraceToken]) (tail json)
    | nextChar == rightBrace    = lexerMakeTokenStream (tokenStream ++ [RightBraceToken]) (tail json)
    | nextChar == leftBracket   = lexerMakeTokenStream (tokenStream ++ [LeftBracketToken]) (tail json)
    | nextChar == rightBracket  = lexerMakeTokenStream (tokenStream ++ [RightBracketToken]) (tail json)
    | nextChar == colon         = lexerMakeTokenStream (tokenStream ++ [ColonToken]) (tail json)
    | nextChar == comma         = lexerMakeTokenStream (tokenStream ++ [CommaToken]) (tail json)
    -- If you encounter a quotation, it is an identifier or text value, drop the quotation and make the identifiertoken with the remaining stream
    | nextChar == quotation     = let textToken = lexerMakeTextToken "" (tail json)
                                                            -- Add 2 to the drop amount, to account for the two quotations that need to be removed.
                                  in  lexerMakeTokenStream (tokenStream ++ [TextToken textToken]) (drop (length textToken + 2) json) 
    -- If you encounter a number, it is a number value.
    | lexerIsJsonNumber nextChar     = let numberToken = lexerMakeNumberToken "" json
                                       in  lexerMakeTokenStream (tokenStream ++ [NumberToken numberToken]) (drop (length numberToken) json)
    | "true" `isPrefixOf` json  = lexerMakeTokenStream (tokenStream ++ [BooleanToken True]) (drop 4 json)
    | "false" `isPrefixOf` json = lexerMakeTokenStream (tokenStream ++ [BooleanToken False]) (drop 5 json)
    | "null" `isPrefixOf` json  = lexerMakeTokenStream (tokenStream ++ [NullToken]) (drop 4 json)
    | otherwise                 = error ("Invalid json, on character: " ++ [nextChar] ++ "\n rest of json input: " ++ json)
    where nextChar = head json

----- Lexer Helper Functions -----
lexerMakeTextToken :: [Char] -> [Char] -> [Char]
lexerMakeTextToken token [] = error "Syntax Error: Json not closed properly"
lexerMakeTextToken token ('\\':'\"':restOfJson) = lexerMakeTextToken (token ++ escapedQuotation) restOfJson
lexerMakeTextToken token json
    | nextChar == quotation = token
    | otherwise = lexerMakeTextToken (token ++ [nextChar]) (tail json)
    where nextChar = head json

lexerMakeNumberToken :: [Char] -> [Char] -> [Char]
lexerMakeNumberToken token [] = error "Syntax Error: Json not closed properly"
lexerMakeNumberToken token json
    | lexerIsJsonNumber nextChar = lexerMakeNumberToken (token ++ [nextChar]) (tail json)
    | otherwise = token
    where nextChar = head json

lexerIsJsonNumber :: Char -> Bool
lexerIsJsonNumber character = character == '.' || isDigit character


----- Parser Functions -----
parseNextToken :: TokenStream -> (ParseTree, TokenStream)
parseNextToken [] = (TextNode "No tokens to parse.", [])
parseNextToken (LeftBraceToken:rest)        = parseObject [] rest
parseNextToken (LeftBracketToken:rest)      = parseArray [] rest
parseNextToken (TextToken text:rest)        = (TextNode text, rest)
parseNextToken (NumberToken number:rest)    = (NumberNode number, rest)
parseNextToken (BooleanToken bool:rest)     = (BooleanNode bool, rest)
parseNextToken (NullToken:rest)             = (NullNode, rest)
-- If the next token does not match to any of the above, the token is incorrect.
parseNextToken invalidTokens                = error ("Error: Next token in stream is not a valid token. Remainder of stream: " ++ show invalidTokens)


parseObject :: [(String, ParseTree)] -> TokenStream -> (ParseTree, TokenStream)
-- BASE CASE: If the next token is a RightBraceToken, the object ends here and needs to be returned, together with the remaining token stream.
parseObject identifiers (RightBraceToken:remainderOfStream) = (JsonObjectNode identifiers, remainderOfStream)
-- If the next token is a TextToken, followed by a ColonToken, it is an identifier.
parseObject identifiers (TextToken identifierKey:ColonToken:rest) = let (identifierValue, remainingObject) = parseNextToken rest
                                                                                            -- SUB-BASE CASE: If there is a RightBraceToken after the identifier, the object closes.
                                                                    in case remainingObject of RightBraceToken:restOfTokenStream    -> (JsonObjectNode (identifiers ++ [(identifierKey , identifierValue)]), restOfTokenStream)
                                                                                            -- If there is a CommaToken after the identifier, add the currect identifier to the object and parse the next identifier with the remaining tokens.
                                                                                               CommaToken:restOfObject              -> parseObject (identifiers ++ [(identifierKey, identifierValue)]) restOfObject
                                                                                               incorrectNextToken                   -> error ("Syntax Error: Expected ',' or '}' after object-identifier with key: " ++ identifierKey)
parseObject identifiers tokens = error ("Syntax Error: Unexpected tokens in object. \n Remainder of tokenstream: " ++ show tokens)


parseArray :: [ParseTree] -> TokenStream -> (ParseTree, TokenStream)
-- BASE CASE: If the next token is a RightBracketToken, the array ends here and needs to be returned, together with the remaining token stream.
parseArray array (RightBracketToken:remainderOfStream) = (ArrayNode array, remainderOfStream)
parseArray array tokenStream = let (arrayElementValue, remainingArray) = parseNextToken tokenStream
                                                         -- SUB-BASE CASE: If there is a RightBracketToken after the element, the array closes.
                               in case remainingArray of RightBracketToken:restOfTokenStream -> (ArrayNode (array ++ [arrayElementValue]), restOfTokenStream)
                                                         -- If there is a CommaToken after the element, add the currect element to the array and parse the next element with the remaining tokens.
                                                         CommaToken:restOfArray              -> parseArray (array ++ [arrayElementValue]) restOfArray
                                                         incorrectNextToken                  -> error "Syntax Error: Expected ',' or ']' in an array"



