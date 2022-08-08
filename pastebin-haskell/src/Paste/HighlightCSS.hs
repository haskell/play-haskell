module Paste.HighlightCSS (
    processHighlightCSS,
) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace)
import Data.List (intercalate)
import Text.Parsec


processHighlightCSS :: IO ByteString
processHighlightCSS = do
    cssl <- parseCSS "highlight-light.pack.css" <$> readFile "highlight-light.pack.css"
    cssd <- parseCSS "highlight-dark.pack.css" <$> readFile "highlight-dark.pack.css"
    let css = namespaceSels (Selector ".clrl") cssl <> namespaceSels (Selector ".clrd") cssd
        result = BS8.pack (writeCSS css)
    result `seq` return result


namespaceSels :: Selector -> CSS -> CSS
namespaceSels sel (CSS items) =
    CSS [(mc, [Block (map (sel <>) sels) rules
              | Block sels rules <- blocks])
        | (mc, blocks) <- items]


newtype CSS = CSS [(Maybe Comment, [Block])]
  deriving (Show)

data Block = Block [Selector] [Rule]
  deriving (Show)

newtype Selector = Selector String
  deriving (Show)

data Rule = Rule String String
  deriving (Show)

newtype Comment = Comment String
  deriving (Show)

instance Semigroup CSS where
    CSS l <> CSS l' = CSS (l ++ l')

instance Semigroup Selector where
    Selector s <> Selector s' = Selector (s ++ ' ' : s')

parseCSS :: FilePath -> String -> CSS
parseCSS fname s =
    case parse (pCSS <* eof) fname s of
      Left err -> error (show err)
      Right res -> res

writeCSS :: CSS -> String
writeCSS (CSS items) =
    intercalate "\n"
        [maybe "" (\(Comment c) -> "/*" ++ c ++ "*/\n") mcomment ++
            concat [intercalate "," [s | Selector s <- sels] ++
                    "{" ++
                    intercalate ";" [k ++ ":" ++ v | Rule k v <- rules] ++
                    "}"
                   | Block sels rules <- blocks]
        | (mcomment, blocks) <- items]

type Parser = Parsec String ()

pCSS :: Parser CSS
pCSS = CSS <$> many ((,) <$> optionMaybe pHeaderComment <*> many1 pBlock)

pHeaderComment :: Parser Comment
pHeaderComment = do
    string' "/*"
    s <- manyTill anyChar (string' "*/")
    void endOfLine
    return (Comment s)

pBlock :: Parser Block
pBlock = do
    sels <- pSelector `sepBy1` (try (whitespace >> char ',') >> whitespace)
    whitespace
    void $ char '{'
    rules <- pRule `sepBy` (try (whitespace >> char ';') >> whitespace)
    whitespace
    void $ char '}'
    whitespace
    return (Block sels rules)

pSelector :: Parser Selector
pSelector = Selector <$> many (noneOf ",{}")

pRule :: Parser Rule
pRule = do
    key <- try (whitespace >> many1 (satisfy (\c -> c `notElem` ":;}" && not (isSpace c))))
    whitespace
    void $ char ':'
    whitespace
    value <- many (noneOf ";}")
    return (Rule key value)

string' :: String -> Parser ()
string' s = void (try (string s))

whitespace :: Parser ()
whitespace = void (many (satisfy isSpace))
