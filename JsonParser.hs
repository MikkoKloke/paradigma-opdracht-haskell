import Text.Parsec (parserBind, tokens)
import Data.Text(replace, pack, unpack)
import Data.Typeable
import Language.Haskell.TH (sourceStrict)
import Distribution.SPDX (LicenseId(MakeIndex))
import qualified Data.Graph as Data
import Text.XHtml (treeColors)
import Distribution.Simple.Utils (lowercase)

import Data.Char(isLetter, isDigit)
import Data.IntMap (size)
import qualified Text.Read.Lex as GHC.Types
-- import Data.ByteString (hPutStr)
-- import Data.ByteString.Char8 (hPutStrLn)


-- TOKENS
number = [1..9] -- not used
alphLwr = ['a'..'z']
alphUpr = ['A'..'Z']
leftBrace = '{'
rightBrace = '}'
quotation = '\"'
colon = ':'
comma = ','

rawJsonInput = "{\n \"Person\" : {\n \"name\" : \"name of person\",\n \"height\" : 180,\n \"weight\" : \"light as a feather\"\n} \n\"packageNr\" :7124627 \n}"

-- TODO!!!!
-- USE A CUSTOM TYPE FOR THE LEXER TOKENSTREAM: *List of different types* :? https://stackoverflow.com/questions/7787317/list-of-different-types
-- TOKEN = PAIR OF ENUM? AND THE VALUE, WHICH CAN BE 2 TYPES:
-- token = (type, (string | int | something else? (Or do i make all the different kinds of tokens different types here?) ))

-- Create new Tree datatype https://dkalemis.wordpress.com/2014/01/23/trees-in-haskell/
-- Data.Tree??
data ParseTree parseTree = EmptyNode
                | Node parseTree [ParseTree parseTree]

-- lexerGetTokenStream :: String -> String -- [String] 
lexerGetTokenStream :: p -> IO ()
lexerGetTokenStream rawJson = do
    let jsonWithoutWhitespace = removeWhiteSpace rawJsonInput -- rawJson 
    -- print jsonWithoutWhitespace
    print (makeTokenStream [] jsonWithoutWhitespace)


makeTokenStream :: [String] -> String -> [String]
makeTokenStream tokenStream json
    | json == "" = tokenStream -- when the json string is empty, give back the tokenstream
    | nextChar == leftBrace = makeTokenStream (tokenStream ++ [[leftBrace]]) (drop 1 json)
    | nextChar == rightBrace = makeTokenStream (tokenStream ++  [[rightBrace]]) (drop 1 json)
    | nextChar == quotation =   let textToken = makeTextToken "" (drop 1 json) -- If you encounter a quotation, it is an identifier, drop the quotation and make the identifiertoken with the remaining stream
                                in  makeTokenStream (tokenStream ++ [textToken]) (drop (length textToken + 2) json) -- 99999 is een placeholder. Weet nog niet hoe ik de drop-amount ga krijgen uit makeIdentifiertoken
    | nextChar == colon = makeTokenStream (tokenStream ++ [[colon]]) (drop 1 json)
    | isJsonNumber nextChar =   let numberToken = makeNumberToken "" json
                                in  makeTokenStream (tokenStream ++ [numberToken]) (drop (length numberToken) json)
    | nextChar == comma = makeTokenStream (tokenStream ++ [[comma]]) (drop 1 json)
    -- | otherwise = tokenStream
    where nextChar = head json

-- makeIdentifierToken :: String -> String
makeTextToken :: [Char] -> [Char] -> [Char]
makeTextToken token json
    | nextChar == quotation = token
    | otherwise = makeTextToken (token ++ [nextChar]) (drop 1 json)
    where nextChar = head json

makeNumberToken :: [Char] -> [Char] -> [Char]
makeNumberToken token json
    | isJsonNumber nextChar || nextChar == '.' = makeNumberToken (token ++ [nextChar]) (drop 1 json) 
    -- | nextChar == '.' = makeNumberToken (token ++ [nextChar]) (drop 1 json)
    | otherwise = token
    where nextChar = head json

isJsonNumber :: Char -> Bool
isJsonNumber character = character == '.' || isDigit character




-- makeTokenStream :: String -> [String] -> [String]
-- makeTokenStream :: [String] -> [String]
-- makeTokenStream json tokenSteam = case json of "" -> [] 
--                                                leftBrace -> tokenStream ++ [leftBrace] ++ makeTokenStream (drop 1 json)
--                                                rightBrace -> stream : rightBrace
--                                                quotation -> tokenStream ++ makeIdentifierToken json

-- makeIdentifierToken :: String -> [String]
-- makeIdentifierToken json =




-- makeTree json tree = do

--     let firstCharacter = take 1 json
--     makeTree firstCharacter tree
--     if firstCharacter == leftBrace || firstCharacter == comma
--         then do
--             putIdentifierIntoTree (drop 1 json) tree
--             -- old
--             let aMyTree = Node firstCharacter (EmptyNode) (EmptyNode)
--             putStrLn "then"
--             makeTree (drop 1 json) aMyTree
--         else
--             if firstCharacter == "\""
--                 then putStrLn "thenTest"
--                 else putStrLn "elseTest"

-- putCharacterIntoTree "{" json tree =
--     makeTree json Node "{" EmptyNode EmptyNode
-- putCharacterIntoTree

-- putIdentifierIntoTree json tree = do
    -- get the identifier token
    -- put identifier token into tree
    -- use makeTree in the node parameters for their trees no?? but yes ??
    -- getIdentifierToken drop 1 json ""

-- getIdentifierToken json token = do
--     let firstCharacter = take 1 json
--     if firstCharacter /= quotation
--         then do
--             let newToken = token ++ firstCharacter
--             getIdentifierToken (drop newToken.length json) newToken
--         else
--             token



-- -
-- -- makeToken json token
-- --     | nextChar == '{' = makeToken (drop 1 json) token
-- --     | isLetter nextChar = take 1 json
-- --     where nextChar = head json

-- -- cycle remainingJson = do
-- --     putStrLn remainingJson
-- --     if null remainingJson
-- --         then
-- --             putStrLn "Empty" -- Print the tree / call the parser
-- --         else do
-- --             Main.cycle (drop 1 remainingJson)


removeWhiteSpace :: [Char] -> String
removeWhiteSpace string = do
    let text = pack string
    unpack (replace (pack "\n") (pack "") (replace (pack " ") (pack "") text))
    



-- tail currenttoken check type

-- GHCI