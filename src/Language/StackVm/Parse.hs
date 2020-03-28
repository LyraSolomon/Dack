module Language.StackVm.Parse (parseFile) where

import Text.Parsec
import Text.IntFormats (intParser)
import Language.StackVm.Syntax
import Data.Char (toUpper, toLower)
import Prelude hiding (Ord(..))

type Parser = Parsec String ()

-- |Parse one or more spaces or tabs
spaces' :: Parser ()
spaces' = oneOf " \t" >> skipMany (oneOf " \t")

-- |Parse zero or more spaces or tabs
optionalSpaces' :: Parser ()
optionalSpaces' = skipMany (oneOf " \t")

-- |Parse the given string in upper, lower, or title case
caseInsensitive :: String -> Parser String
caseInsensitive s = try (string (map toLower s)) <|>
                    try (string (map toUpper s)) <|>
                    try (string ((\(x:xs) -> toUpper x : map toLower xs)s))

-- |Parse memory segment of the form [ argument | local | static | this | that | pointer | temp ] in upper, lower, or title case
parseSegment :: Parser Segment
parseSegment = (try (caseInsensitive "argument") >> return Argument) <|>
               (try (caseInsensitive "local")    >> return Local) <|>
               (try (caseInsensitive "static")   >> return Static) <|>
               (try (caseInsensitive "constant") >> return Constant) <|>
               (try (caseInsensitive "this")     >> return This) <|>
               (try (caseInsensitive "that")     >> return That) <|>
               (try (caseInsensitive "pointer")  >> return Pointer) <|>
               (try (caseInsensitive "temp")     >> return Temp) <?>
               "name of memory segment (argument, local, static, constant, this, that, pointer, temp) like this, THIS, or This"

-- |Parse command of the form [ add | sub | neg | eq | gt | lt | and | or | not | push __segment__ __int__ | pop __segment__ __int__ |
--  label __identifier__ | goto __identifier__ | if-goto __identifier__ | function __identifier__ __int__ | call __identifier__ __int__ | return ]
parseCommand :: Parser Command
parseCommand = do {
    try (caseInsensitive "add");
    return Add;
  } <|> do {
    try (caseInsensitive "sub");
    return Sub
  } <|> do {
    try (caseInsensitive "neg");
    return Neg
  } <|> do {
    try (caseInsensitive "eq");
    return Eq
  } <|> do {
    try (caseInsensitive "gt");
    return Gt
  } <|> do {
    try (caseInsensitive "lt");
    return Lt
  } <|> do {
    try (caseInsensitive "and");
    return And
  } <|> do {
    try (caseInsensitive "or");
    return Or
  } <|> do {
    try (caseInsensitive "not");
    return Not
  } <|> do {
    try (caseInsensitive "push");
    spaces';
    srcSegment <- parseSegment;
    spaces';
    Push srcSegment <$> intParser
  } <|> do {
    try (caseInsensitive "pop");
    spaces';
    dstSegment <- parseSegment;
    spaces';
    Pop dstSegment <$> intParser
  } <|> do {
    try (caseInsensitive "label");
    char ' ';
    Label <$> parseLabel
  } <|> do {
    try (caseInsensitive "goto");
    char ' ';
    Goto <$> parseLabel
  } <|> do {
    try (caseInsensitive "if" >> char '-' >> caseInsensitive "goto");
    char ' ';
    IfGoto <$> parseLabel
  } <|> do {
    try (caseInsensitive "function");
    spaces';
    fname <- parseLabel;
    spaces';
    Function fname <$> intParser
  } <|> do {
    try (caseInsensitive "call");
    spaces';
    name <- parseLabel;
    spaces';
    Call name <$> intParser
  } <|> do {
    try (caseInsensitive "return");
    return Return
  } <?> "stack operation (add, sub, eq, gt, lt, and, or, not, neg, push, pop, label, goto, if-goto, function, call, or return)"

-- |Parse identifier of the form [a-zA-Z_.:][a-zA-Z0-9_.:]*
parseLabel :: Parser String
parseLabel = do {
    x <- letter <|> oneOf "_.:";
    xs <- many $ alphaNum <|> oneOf "_.:";
    return (x:xs)
  } <?> "valid label [a-zA-Z_.:][a-zA-Z0-9_.:]*"

-- |Parse line of the form [ __command__? __comment__? ] where comments are of the form @//.*@
parseLine :: Parser Command
parseLine = do {
    optionalSpaces';
    s <- option Comment parseCommand;
    optionalSpaces';
    optional (string "//" >> many (noneOf "\n"));
    return s
  }

-- |Parse file
fileParser :: Parser [Command]
fileParser = do { s <- parseLine `sepBy` endOfLine; eof; return s }

-- |Run the file parser
parseFile :: String -> Either String [Command]
parseFile = mapLeft show . parse fileParser ""

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
