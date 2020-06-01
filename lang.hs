import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token 
import Text.Parsec.Language
import Text.Parsec.Expr 
import Control.Monad.State 
import Control.Monad.Except 
import qualified Data.Map as M
import System.IO


-- Parser

data Expression = Constant Double 
                | Addition Expression Expression
                | Division Expression Expression
                | Multiplication Expression Expression
                | Subtraction Expression Expression
                | Modulus Expression Expression
                | Negation Expression 
                | Identifier String
                | FunctionInvocation String Expression
                | Conditional Condition Expression Expression
                | Loop Condition Expression
                deriving (Show)

data Condition = And Condition Condition
                | Or Condition Condition
                | Not Condition
                | Equal Expression Expression
                | LessThan Expression Expression
                | LessThanEqual Expression Expression
                deriving (Show)

data FunctionBody = FunctionBody String Expression deriving (Show)

data Statement = PrintStatement Expression
                | AssignmentStatement String Expression
                | FunctionDefinition String FunctionBody
                | WhiteSpace
                 deriving (Show)



lexer :: TokenParser ()
lexer = makeTokenParser (emptyDef  { opStart = oneOf "+-*|&=!<>", opLetter = oneOf "+-*|&=!<>", reservedNames = ["real", "writeln", "function", "if", "then", "else", "while", "do"], reservedOpNames = ["mod", "div"], caseSensitive = True})

parseNumber :: Parser Expression
parseNumber = do
    val <- naturalOrFloat lexer
    case val of
        Left i -> return $ Constant $ fromInteger i 
        Right n -> return $ Constant $ n


parseExpression :: Parser Expression
parseExpression = (flip buildExpressionParser) parseTerm $ [
    [ Prefix (reservedOp lexer "-" >> return Negation), Prefix (reservedOp lexer "+" >> return id) ],
    [ Infix (reservedOp lexer "*" >> return Multiplication )AssocLeft, Infix (reservedOp lexer "div" >> return Division )AssocLeft, Infix (reservedOp lexer "mod" >> return Modulus )AssocLeft ],
    [ Infix (reservedOp lexer "+" >> return Addition )AssocLeft, Infix (reservedOp lexer "-" >> return Subtraction )AssocLeft ]]

parseFunctionCall :: Parser Expression
parseFunctionCall = do
    ident <- identifier lexer
    expr <- parens lexer parseExpression
    return $ FunctionInvocation ident expr

parseConditional :: Parser Expression
parseConditional = do
    reserved lexer "if"
    c <- parseCondition
    reserved lexer "then"
    e1 <- parseExpression
    reserved lexer "else"
    e2 <- parseExpression
    return $ Conditional c e1 e2

parseLoop :: Parser Expression
parseLoop = do
    reserved lexer "while"
    c <- parseCondition
    reserved lexer "do"
    e <- parseExpression
    return $ Loop c e



parseCondition :: Parser Condition
parseCondition = (flip buildExpressionParser) parseConditionalTerm $ [
    [ Prefix (reservedOp lexer "!" >> return Not) ],
    [ Infix (reservedOp lexer "&&" >> return And) AssocLeft, 
    Infix (reservedOp lexer "||" >> return Or) AssocLeft ]]

parseConditionalTerm :: Parser Condition
parseConditionalTerm = 
    parens lexer parseCondition
    <|> parseComparison


parseComparison :: Parser Condition
parseComparison = do 
    e1 <- parseExpression
    f <- (reserved lexer "=" >> return (Equal e1)) 
         <|> (reserved lexer "<" >> return (LessThan e1)) 
         <|> (reserved lexer "<=" >> return (LessThanEqual e1))
         <|> (reserved lexer ">" >> return (Not . (LessThanEqual e1)))
         <|> (reserved lexer ">=" >> return (Not . (LessThan e1)))
         <|> (reserved lexer "!=" >> return (Not . (Equal e1)))
    e2 <- parseExpression
    return $ f e2

parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression 
            <|> parseNumber 
            <|> try parseConditional 
            <|> try parseLoop 
            <|> try parseFunctionCall
            <|> (identifier lexer >>= return . Identifier)

parsePrint :: Parser Statement
parsePrint = do
    reserved lexer "writeln"
    expr <- parseExpression
    reservedOp lexer ";"
    return $ PrintStatement expr

parseWhiteSpace :: Parser Statement
parseWhiteSpace = do 
    whiteSpace lexer
    return WhiteSpace

parseAssignment :: Parser Statement
parseAssignment = do
    reserved lexer "real"
    identifier <- identifier lexer
    reservedOp lexer ":="
    expr <- parseExpression
    reservedOp lexer ";"
    return $ AssignmentStatement identifier expr

parseFunctionDefinition :: Parser Statement
parseFunctionDefinition = do
    reserved lexer "function"
    ident <- identifier lexer
    argname <- parens lexer $ identifier lexer
    reservedOp lexer ":="
    expr <- parseExpression
    reservedOp lexer ";"
    return $ FunctionDefinition ident (FunctionBody argname expr)

parseLanguage :: Parser Statement
parseLanguage = do 
    whiteSpace lexer
    s <- ( parsePrint <|> parseAssignment <|> parseFunctionDefinition <|> parseWhiteSpace)
    eof
    return s


-- Interpreter

type StoredVal = Either Double FunctionBody

type FFLanguage = StateT (M.Map String StoredVal) IO  
type Language a = ExceptT String FFLanguage a

interpretCondition :: Condition -> Language Bool
interpretCondition (Not c ) = interpretCondition c >>= return . not
interpretCondition (And c1 c2 ) = do
    b1 <- interpretCondition c1
    b2 <- interpretCondition c2
    return (b1 && b2)
interpretCondition (Or c1 c2 ) = do
    b1 <- interpretCondition c1
    b2 <- interpretCondition c2
    return (b1 || b2)
interpretCondition (Equal c1 c2 ) = do
    v1 <- interpretExpression c1
    v2 <- interpretExpression c2
    return (v1 == v2)
interpretCondition (LessThan c1 c2 ) = do
    v1 <- interpretExpression c1
    v2 <- interpretExpression c2
    return (v1 < v2)
interpretCondition (LessThanEqual c1 c2 ) = do
    v1 <- interpretExpression c1
    v2 <- interpretExpression c2
    return (v1 <= v2)

interpretExpression :: Expression -> Language Double
interpretExpression (Constant n) = return n
interpretExpression (Identifier i) = do
    varmap <- get
    case  M.lookup i varmap of
        Nothing -> fail ("Unknown identifier: " ++ i)
        Just (Left n) -> return n
        Just (Right _) -> fail("Function call error: " ++ i)

interpretExpression (Addition e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    return (v1 + v2)
interpretExpression (Subtraction e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    return (v1 - v2)
interpretExpression (Division e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    return (v1 / v2)
interpretExpression (Multiplication e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    return (v1 * v2)
interpretExpression (Negation e1 ) = do
    v1 <- interpretExpression e1
    return $ negate v1
interpretExpression (Modulus e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    let n1 = floor v1
        n2 = floor v2
        m = n1 `mod` n2
    return $ fromInteger m
interpretExpression (FunctionInvocation fn e) = do
    ctx <- get
    case M.lookup fn ctx of
        Nothing -> fail("Function not defined : " ++ fn)
        Just (Left _) -> fail ("Constant call")
        Just (Right (FunctionBody argname expr)) -> do
            n <- interpretExpression e
            modify (M.insert argname (Left n))
            r <- interpretExpression expr
            put ctx
            return r

interpretExpression (Conditional cond e1 e2) = do
    b <- interpretCondition cond
    if b then interpretExpression e1 else interpretExpression e2

interpretExpression (Loop cond e1 ) = do
    b <- interpretCondition cond
    if b then interpretExpression e1 else interpretExpression e1


interpretStatement :: Statement -> Language ()
interpretStatement (PrintStatement expr) = do
    n <- interpretExpression expr
    liftIO $ print n
interpretStatement (AssignmentStatement ident expr) = do
    n <- interpretExpression expr
    modify (M.insert ident (Left n))
interpretStatement (FunctionDefinition fn body) = do 
    modify (M.insert fn (Right body))
interpretStatement (WhiteSpace) = do
    return ()
    

varsTable :: M.Map String StoredVal
varsTable = M.fromList []

interpretLanguage :: String -> FFLanguage () 
interpretLanguage s = 
    case ret of
        Left e -> liftIO $  putStrLn $ "error: " ++ (show e)
        Right n -> do res <- runExceptT $ interpretStatement n 
                      case res of
                          Left e' -> liftIO $ putStrLn $ "error: " ++ e'
                          Right _ -> return ()
    where
        ret = parse parseLanguage "" s

language :: FFLanguage () 
language =  liftIO (readFile "t.txt") >>= (mapM_ interpretLanguage) . lines


main :: IO () 
main = evalStateT language varsTable