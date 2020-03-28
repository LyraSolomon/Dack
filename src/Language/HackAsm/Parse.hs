module Language.HackAsm.Parse (parseFile) where

import Text.Parsec
import Text.IntFormats (intParser)
import Language.HackAsm.Syntax

type Parser = Parsec String ()

-- |Parse zero or more spaces or tabs
spaces' :: Parser ()
spaces' = skipMany (oneOf " \t")

-- |Parse statement of the form [ @__address__ | (__label__) | __registers__ = __op__ __jump__ ]
parseStatement :: Parser Statement
parseStatement = do {
    _ <- char '@';
    spaces';
    Instruction . AStatement <$> parseAddress;
  } <|> do {
    _ <- char '(';
    spaces';
    name <- parseLabel;
    spaces';
    _ <- char ')';
    return $ LStatement name
  } <|> do {
    regList <- parseRegisterList;
    spaces';
    op <- parseOperation;
    spaces';
    Instruction . CStatement regList op <$> parseJump
  } <?> "AStatement @address, CStatement [registers=]? operation [;jump]?, or label (label)"

-- |Parse address of the form [ __integer__ | __label__ ]
parseAddress :: Parser Address
parseAddress = (Relative <$> parseLabel) <|> (Literal <$> intParser)

-- |Parse registers of the form [ __empty__ | a?d?m?= ] case and order insensitive
parseRegisterList :: Parser RegisterList
parseRegisterList = option (StoreADM False False False) (try $ do {
    registers <- many (oneOf "ADMadm ");
    _ <- char '=';
    if 'A' `elem` registers || 'a' `elem` registers
    then if 'D' `elem` registers || 'd' `elem` registers
         then if 'M' `elem` registers || 'm' `elem` registers
              then return $ StoreADM True  True  True
              else return $ StoreADM True  True  False
         else if 'M' `elem` registers || 'm' `elem` registers
              then return $ StoreADM True  False True
              else return $ StoreADM True  False False
    else if 'D' `elem` registers || 'd' `elem` registers
         then if 'M' `elem` registers || 'm' `elem` registers
              then return $ StoreADM False True  True
              else return $ StoreADM False True  False
         else if 'M' `elem` registers || 'm' `elem` registers
              then return $ StoreADM False False True
              else return $ StoreADM False False False
  })

-- |Parse operation of the form [ -__val__ | !__val__ | __val__ - __val__ | __val__ + __val__ | __val__ \\| __val__ | __val__ & __val__ | __val__ ]
parseOperation :: Parser Operation
parseOperation = do {
    _ <- char '-';
    spaces';
    Negate <$> parseValue
  } <|> do {
    _ <- char '!';
    spaces';
    Not <$> parseValue
  } <|> do {
    x <- parseValue;
    spaces';
    option (Constant x) (do {
      _ <- char '-';
      spaces';
      Subtract x <$> parseValue
    } <|> do {
      _ <- char '+';
      spaces';
      Add x <$> parseValue
    } <|> do {
      _ <- char '|';
      spaces';
      Or x <$> parseValue
    } <|> do {
      _ <- char '&';
      spaces';
      And x <$> parseValue
    } <?> "operation (Value, -Value, !Value, Value1+Value2, Value1-Value2, Value1|Value2, or Value1&Value2)")
  } <?> "operation (Value, -Value, !Value, Value1+Value2, Value1-Value2, Value1|Value2, or Value1&Value2)"

-- |Parse value of the form [ 0 | 1 | A | D | M ] case insensitive
parseValue :: Parser Value
parseValue = (char '0' >> return Const0) <|>
             (char '1' >> return Const1) <|>
             (char 'A' >> return RegisterA) <|>
             (char 'D' >> return RegisterD) <|>
             (char 'M' >> return RegisterM) <|>
             (char 'a' >> return RegisterA) <|>
             (char 'd' >> return RegisterD) <|>
             (char 'm' >> return RegisterM) <?> "value (constant 0 or 1, or register A, D, or M, case insensitive)"

-- |Parse jump of the form [ empty | ; jmp | ; jlt | ; jgt | ; jle | ; jge | ; jeq | ; jne ] in upper, lower, or title case
parseJump :: Parser Jump
parseJump = option Continue (do {
    _ <- char ';';
    spaces';
    option Continue (try ( string "JMP" >> return JMP) <|>
                     try ( string "Jmp" >> return JMP) <|>
                     try ( string "jmp" >> return JMP) <|>

                     try ( string "JLT" >> return JLT) <|>
                     try ( string "Jlt" >> return JLT) <|>
                     try ( string "jlt" >> return JLT) <|>

                     try ( string "JGT" >> return JGT) <|>
                     try ( string "Jgt" >> return JGT) <|>
                     try ( string "jgt" >> return JGT) <|>

                     try ( string "JLE" >> return JLE) <|>
                     try ( string "Jle" >> return JLE) <|>
                     try ( string "jle" >> return JLE) <|>

                     try ( string "JGE" >> return JGE) <|>
                     try ( string "Jge" >> return JGE) <|>
                     try ( string "jge" >> return JGE) <|>

                     try ( string "JEQ" >> return JEQ) <|>
                     try ( string "Jeq" >> return JEQ) <|>
                     try ( string "jeq" >> return JEQ) <|>

                     try ( string "JNE" >> return JNE) <|>
                     try ( string "Jne" >> return JNE) <|>
                     try ( string "jne" >> return JNE) <?> "jump statement (empty, or one of jmp, jlt, jgt, jle, jge, jeq, or jne, may be capitalized like THIS, This, or this)")
  })

-- |Parse label of the form @[a-zA-Z_.$:][a-zA-Z0-9_.$:]*@
parseLabel :: Parser Label
parseLabel = do {
    x <- letter <|> oneOf "_.$:";
    xs <- many $ alphaNum <|> oneOf "_.$:";
    return $ AddrLabel (x:xs)
  } <?> "valid label [a-zA-Z_.$:][a-zA-Z0-9_.$:]*"

-- |Parse line of the form [ __statement__? __comment__? ] where comments are of the form @//.*@
parseLine :: Parser Statement
parseLine = do {
    spaces';
    s <- option Comment parseStatement;
    spaces';
    optional (string "//" >> many (noneOf "\n"));
    return s
  }

-- |Parse file
fileParser :: Parser [Statement]
fileParser = do { s <- parseLine `sepBy` endOfLine; eof; return s }

-- |Run the file parser
parseFile :: String -> Either String [Statement]
parseFile = mapLeft show . parse fileParser ""

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
