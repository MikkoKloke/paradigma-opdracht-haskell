import Text.Parsec (parserBind)
import Data.Text(replace, pack, unpack)
import Data.Typeable
import Language.Haskell.TH (sourceStrict)
import Distribution.SPDX (LicenseId(MakeIndex))
import qualified Data.Graph as Data
import Text.XHtml (treeColors)
import Distribution.Simple.Utils (lowercase)

import Data.Char(isLetter)
import Data.IntMap (size)
-- import Data.ByteString (hPutStr)
-- import Data.ByteString.Char8 (hPutStrLn)


-- TOKENS
number = [1..9]
alphLwr = ['a'..'z']
alphUpr = ['A'..'Z']
leftBrace = "{"
rightBrace = "}"
quotation = "\""
colon = ":"
comma = ","

rawJsonInput = "{\n \"Person\" : {\n \"name\" : \"name of person\",\n \"height\" : 1.80,\n \"weight\" : \"light as a feather\"\n} \n\"packageNr\" :1724627 \n}"

-- Create new Tree datatype https://dkalemis.wordpress.com/2014/01/23/trees-in-haskell/
-- Data.Tree??
data ParseTree parseTree = EmptyNode
                | Node parseTree [ParseTree parseTree]

-- lexerGetTokenStream :: String -> String -- [String] 
lexerGetTokenStream rawJson = -- do
     removeWhiteSpace rawJsonInput-- rawJson -- let jsonWithoutWhitespace = 
    -- makeTokenStream jsonWithoutWhitespace []


makeTokenStream :: [String] -> [String] -> t
makeTokenStream tokenStream json
    | nextChar == leftBrace = makeTokenStream (tokenStream ++ [leftBrace]) (drop 1 json) 
    | nextChar == rightBrace = makeTokenStream (tokenStream ++  [rightBrace]) (drop 1 json)
    | nextChar == quotation =   let identifiertoken = makeIdentifierToken "" (drop 1 json)
                                in  makeTokenStream [identifiertoken] (drop (identifiertoken.length + 2) json) -- 99999 is een placeholder. Weet nog niet hoe ik de drop-amount ga krijgen uit makeIdentifiertoken
    where nextChar = head json

-- makeIdentifierToken :: String -> String
makeIdentifierToken token json
    | nextChar == quotation = token
    | otherwise = makeIdentifierToken (token ++ nextChar) (drop 1 json)
    where nextChar = head json

    -- | nextChar == alphLwr =  makeIdentifierToken ((token ++ nextChar) (drop 1 json))








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


removeWhiteSpace :: String -> String
removeWhiteSpace string = do
    let text = pack string
    unpack (replace (pack "\n") (pack "") (replace (pack " ") (pack "") text))
    



-- tail currenttoken check type

-- GHCI