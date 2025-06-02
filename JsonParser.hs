import Text.Parsec (parserBind, tokens, string, tab)
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
import Distribution.FieldGrammar (Token)
import Graphics.Win32 (ShowWindowControl)
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

rawJsonInput = "{\n \"Destination\" : \"Japan\", \"Person\" : {\n \"name\" : \"Carlo\",\n \"height\" : 150,\n \"weight\" : \"light as a feather\"\n} \n\"packageNr\" :7124627 \n}"

-- TODO!!!!
-- USE A CUSTOM TYPE FOR THE LEXER TOKENSTREAM: *List of different types* :? https://stackoverflow.com/questions/7787317/list-of-different-types
-- TOKEN = PAIR OF ENUM? AND THE VALUE, WHICH CAN BE 2 TYPES:
-- token = (type, (string | int | something else? (Or do i make all the different kinds of tokens different types here?) ))
data LexerToken = LeftBraceToken
                | RightBraceToken
                | ColonToken
                | CommaToken
                | TextToken String 
                | NumberToken String
                deriving (Show)-- NumberToken INT OR FLOAT?

type TokenStream = [LexerToken]

-----------------------------------

-------- Maybe good for pareser, not lexer --------
-- data AdvancedLexerToken = ObjectToken Char | IdentifierToken Identifier

-- data IdentifierValue = TextValue String
--                     | NumberValue String
--                     | ArrayValue [IdentifierValue]
--                     | ObjectValue [Identifier]
--                     | BooleanValue Bool
--                     -- | NullValue - try later
--                     deriving Show

-- data Identifier = Identifier {
--     key :: String,
--     value :: IdentifierValue
-- } deriving Show




-- Create new Tree datatype https://dkalemis.wordpress.com/2014/01/23/trees-in-haskell/ - https://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures
-- Data.Tree??
data ParseTree parseTree = EmptyNode
                | Node parseTree [parseTree] deriving Show

-- lexerGetTokenStream :: String -> String -- [String] 
lexerGetTokenStream :: String -> IO ()
lexerGetTokenStream rawJson = do
    let jsonWithoutWhitespace = removeWhiteSpace rawJson
    print jsonWithoutWhitespace
    let tokenStream = lexerMakeTokenStream [] jsonWithoutWhitespace 
    print tokenStream
    let parseTree = makeParseTree (EmptyNode :: ParseTree()) tokenStream
    print parseTree

-- lexerMakeTokenStream :: [String] -> String -> [String]
lexerMakeTokenStream :: TokenStream -> String -> TokenStream
lexerMakeTokenStream tokenStream json
    -- Base case for the recursion, if all the json is tokenified give back the tokenStream.
    | null json = tokenStream
    -- Simple 1 character tokens.
    | nextChar == leftBrace = lexerMakeTokenStream (tokenStream ++ [LeftBraceToken]) (tail json)
    | nextChar == rightBrace = lexerMakeTokenStream (tokenStream ++  [RightBraceToken]) (tail json)
    | nextChar == colon = lexerMakeTokenStream (tokenStream ++ [ColonToken]) (tail json)
    | nextChar == comma = lexerMakeTokenStream (tokenStream ++ [CommaToken]) (tail json)
    -- If you encounter a quotation, it is an identifier or text value, drop the quotation and make the identifiertoken with the remaining stream
    | nextChar == quotation =   let textToken = lexerMakeTextToken "" (tail json)
                                in  lexerMakeTokenStream (tokenStream ++ [TextToken textToken]) (drop (length textToken + 2) json) -- Add 2 to the drop amount, to account for the two quotations that need to be removed. 
    -- If you encounter a number, it is a number value
    | isJsonNumber nextChar =   let numberToken = lexerMakeNumberToken "" json
                                in  lexerMakeTokenStream (tokenStream ++ [NumberToken numberToken]) (drop (length numberToken) json)
    -- | otherwise = tokenStream
    where nextChar = head json

makeParseTree :: ParseTree t -> TokenStream -> ParseTree t
makeParseTree tree tokenStream = tree
    where nextToken = head tokenStream

addNodeToParseTree ::   ParseTree t -> LexerToken -> ParseTree t
addNodeToParseTree tree LeftBraceToken      = tree
addNodeToParseTree tree RightBraceToken     = tree
addNodeToParseTree tree ColonToken          = tree
addNodeToParseTree tree CommaToken          = tree
addNodeToParseTree tree (TextToken value)   = tree
addNodeToParseTree tree (NumberToken value) = tree

    ---------------- CODE WITHOUT CUSTOM TOKENS ----------------
    -- lexerMakeTokenStream tokenStream json
    -- -- Base case for the recursion, if all the json is tokenified give back the tokenStream.
    -- | null json = tokenStream
    -- -- Simple 1 character tokens.
    -- | nextChar == leftBrace = lexerMakeTokenStream (tokenStream ++ [[leftBrace]]) (tail json)
    -- | nextChar == rightBrace = lexerMakeTokenStream (tokenStream ++  [[rightBrace]]) (tail json)
    -- | nextChar == colon = lexerMakeTokenStream (tokenStream ++ [[colon]]) (tail json)
    -- | nextChar == comma = lexerMakeTokenStream (tokenStream ++ [[comma]]) (tail json)
    -- -- If you encounter a quotation, it is an identifier, drop the quotation and make the identifiertoken with the remaining stream
    -- | nextChar == quotation =   let textToken = lexerMakeTextToken "" (tail json)
    --                             in  lexerMakeTokenStream (tokenStream ++ [textToken]) (drop (length textToken + 2) json) -- Add 2 to the drop amount, to account for the two quotations that need to be removed. 
    
    -- | isJsonNumber nextChar =   let numberToken = lexerMakeNumberToken "" json
    --                             in  lexerMakeTokenStream (tokenStream ++ [numberToken]) (drop (length numberToken) json)
    -- -- | otherwise = tokenStream
    -- where nextChar = head json

-- lexerMakeTextToken :: [Char] -> [Char] -> [Char]
lexerMakeTextToken token json
    | nextChar == quotation = token
    | otherwise = lexerMakeTextToken (token ++ [nextChar]) (tail json)
    where nextChar = head json



-- lexerMakeNumberToken :: [Char] -> [Char] -> [Char]
lexerMakeNumberToken token json
    | isJsonNumber nextChar || nextChar == '.' = lexerMakeNumberToken (token ++ [nextChar]) (tail json) 
    -- | nextChar == '.' = makeNumberToken (token ++ [nextChar]) (drop 1 json)
    | otherwise = token
    where nextChar = head json

isJsonNumber :: Char -> Bool
isJsonNumber character = character == '.' || isDigit character

removeWhiteSpace :: [Char] -> String
removeWhiteSpace string = do
    let text = pack string
    unpack (replace (pack "\n") (pack "") (replace (pack " ") (pack "") text))
    



-- tail currenttoken check type

-- GHCI