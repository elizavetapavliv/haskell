{-# LANGUAGE TupleSections #-}
module Interpr where

import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List as L (unfoldr, stripPrefix, find)
import Data.Map as M (Map, fromList, lookup)
import Data.Maybe (isJust, isNothing)
import Control.Monad (join)
import Data.Fixed (mod')
import Control.Exception
import Data.Map((!))

data Token = TokenOpenPar
           | TokenClosePar
           | TokenPlus
           | TokenMinus
           | TokenAsterisk
           | TokenSlash
           | TokenBackSlash
           | TokenPercent
           | TokenWedge
           | TokenAbs
           | TokenLn
           | TokenExp
           | TokenFloat Float
           | TokenIdent String
           | TokenJmp
           | TokenJmpIf
  deriving (Show)
  
data InterprException = IllegalCharacterException String 
                      | IndexOutOfBoundsException Int 
  deriving (Show)
  
instance Exception InterprException  

type TokenSelector = String -> Maybe (String, String)
type TokenReader = String -> Token
type TokenAcceptor = (TokenSelector, TokenReader)
type FixedTokenDescriptor = (String, Token)
type CharCategoryTokenDescriptor = ((Char -> Bool), String -> Token)

fixedTokenDescriptors :: [FixedTokenDescriptor]
fixedTokenDescriptors = [
  ("(",     TokenOpenPar),
  (")",     TokenClosePar),
  ("+",     TokenPlus),
  ("-",     TokenMinus),
  ("*",     TokenAsterisk),
  ("/",     TokenSlash),
  ("\\",    TokenBackSlash),
  ("%",     TokenPercent),
  ("^",     TokenWedge),
  ("abs",   TokenAbs),
  ("ln",    TokenLn),
  ("exp",   TokenExp),
  ("jmp",   TokenJmp),
  ("jmpIf", TokenJmpIf)
  ]

makeFixedTokenAcceptor :: FixedTokenDescriptor -> TokenAcceptor
makeFixedTokenAcceptor (s, t) = (fmap (s,) . (stripPrefix s), const t)

fixedTokenAcceptors = map makeFixedTokenAcceptor fixedTokenDescriptors

isFloat :: Char -> Bool
isFloat c = isDigit c || c == '.'

charCategoryTokenDescriptors :: [CharCategoryTokenDescriptor]
charCategoryTokenDescriptors = [
  (isFloat, TokenFloat . read),
  (isAlpha, TokenIdent)
  ]

maybeNonEmpty :: [a] -> Maybe [a]
maybeNonEmpty [] = Nothing
maybeNonEmpty xs = Just xs

makeCharCategoryTokenAcceptor :: CharCategoryTokenDescriptor -> TokenAcceptor
makeCharCategoryTokenAcceptor (p, f) = (\s -> let (s1, s2) = span p s in fmap (,s2) $ maybeNonEmpty s1, f)

charCategoryTokenAcceptors = map makeCharCategoryTokenAcceptor charCategoryTokenDescriptors

tokenAcceptors = fixedTokenAcceptors ++ charCategoryTokenAcceptors

mapTokenAcceptors :: String -> [Maybe (Token, String)]
mapTokenAcceptors s = map (\(f, g) -> fmap (first g) $ f s) tokenAcceptors

isToken :: Maybe (Token, String) -> Bool
isToken t = isJust t && fmap snd t == Just ""

acceptToken :: String -> Maybe (Token, String)
acceptToken s = join $ find isToken $ mapTokenAcceptors s

getTokens :: [String] -> [[Token]]
getTokensHelper :: [String] -> [[Token]] -> [[Token]]
getTokensHelper [] acc = acc
getTokensHelper (x:xs) acc
     | all isNothing (mapTokenAcceptors x) = throw (IllegalCharacterException x)
     | otherwise = getTokensHelper xs (acc ++ [unfoldr acceptToken x])
getTokens xs = getTokensHelper xs []

tokenize :: String -> [Token]
tokenize = concat . getTokens . words

data UnOp = UnOpNegate
          | UnOpAbs
          | UnOpLn
          | UnOpExp  
          | UnOpJmp
  deriving (Show, Eq, Ord)

data BinOp = BinOpAdd
           | BinOpSub
           | BinOpMul
           | BinOpDiv
           | BinOpMod
           | BinOpPow
           | BinOpRoot
           | BinOpJmpIf
  deriving (Show, Eq, Ord)

data Expression = ExConst Float
                | ExVar String
                | ExUnary UnOp Expression
                | ExBinary BinOp Expression Expression
  deriving (Show)

type PartialParse a = (a, [Token])
type MaybeParse a = Maybe (PartialParse a)
type Parser a = [Token] -> MaybeParse a
type Expectation a = Token -> Maybe a
type BinOpExpectation = Expectation BinOp

expectAdditiveOp :: BinOpExpectation
expectAdditiveOp TokenPlus = Just BinOpAdd
expectAdditiveOp TokenMinus = Just BinOpSub
expectAdditiveOp _ = Nothing

expectMultiplicativeOp :: BinOpExpectation
expectMultiplicativeOp TokenAsterisk = Just BinOpMul
expectMultiplicativeOp TokenSlash = Just BinOpDiv
expectMultiplicativeOp TokenPercent = Just BinOpMod
expectMultiplicativeOp _ = Nothing

expectPowerOp :: BinOpExpectation
expectPowerOp TokenWedge = Just BinOpPow
expectPowerOp TokenBackSlash = Just BinOpRoot
expectPowerOp _ = Nothing

expectJmpIfOp :: BinOpExpectation
expectJmpIfOp TokenJmpIf = Just BinOpJmpIf
expectJmpIfOp _ = Nothing

expectUnOp :: Expectation UnOp
expectUnOp TokenMinus = Just UnOpNegate
expectUnOp TokenAbs = Just UnOpAbs
expectUnOp TokenLn = Just UnOpLn
expectUnOp TokenExp = Just UnOpExp
expectUnOp TokenJmp = Just UnOpJmp
expectUnOp _ = Nothing

expectInt :: Expectation Float
expectInt (TokenFloat x) = Just x
expectInt _ = Nothing

expectIdent :: Expectation String
expectIdent (TokenIdent v) = Just v
expectIdent _ = Nothing

expectOpenPar :: Expectation ()
expectOpenPar (TokenOpenPar) = Just ()
expectOpenPar _ = Nothing

expectClosePar :: Expectation ()
expectClosePar (TokenClosePar) = Just ()
expectClosePar _ = Nothing

parseSingleToken :: (Token -> Maybe a) -> Parser a
parseSingleToken _ [] = Nothing
parseSingleToken f (t:ts) = fmap (,ts) $ f t

parseOpAndNextOperand :: BinOpExpectation -> Parser Expression -> Parser (BinOp, Expression)
parseOpAndNextOperand opf exf ts0 = do
  (op, ts1) <- parseSingleToken opf ts0
  (ex, ts2) <- exf ts1
  return ((op, ex), ts2)

parseBinOpSequence :: Parser Expression -> BinOpExpectation -> Parser Expression
parseBinOpSequence exf opf = (fmap $ parseBinOpSequence2 exf opf) . exf

parseBinOpSequence2 :: Parser Expression -> BinOpExpectation -> PartialParse Expression -> PartialParse Expression
parseBinOpSequence2 exf opf a@(ex1, ts) = (maybe a (parseBinOpSequence3 exf opf ex1)) $ parseOpAndNextOperand opf exf ts

parseBinOpSequence3 :: Parser Expression -> BinOpExpectation -> Expression -> PartialParse (BinOp, Expression) -> PartialParse Expression
parseBinOpSequence3 exf opf ex1 ((op, ex2), ts) = parseBinOpSequence2 exf opf (ExBinary op ex1 ex2, ts)

parseSum :: Parser Expression
parseSum = parseBinOpSequence parseProduct expectAdditiveOp

parseProduct :: Parser Expression
parseProduct = parseBinOpSequence parsePower expectMultiplicativeOp

parsePower :: Parser Expression
parsePower = parseBinOpSequence parseJmpIfOp expectPowerOp

parseJmpIfOp :: Parser Expression
parseJmpIfOp = parseBinOpSequence parseTerm expectJmpIfOp

parseAlternatives :: [Parser Expression] -> Parser Expression
parseAlternatives fs ts = join $ find isJust $ map (\f -> f ts) fs

parseTerm :: Parser Expression
parseTerm = parseAlternatives [parseUnaryOpAndBareTerm, parseBareTerm]

parseUnaryOpAndBareTerm :: Parser Expression
parseUnaryOpAndBareTerm ts0 = do
  (op, ts1) <- parseSingleToken expectUnOp ts0
  (ex, ts2) <- parseBareTerm ts1
  return (ExUnary op ex, ts2)

parseBareTerm :: Parser Expression
parseBareTerm = parseAlternatives [parseConst, parseVar, parseSubexpression]

parseConst :: Parser Expression
parseConst = (fmap (first ExConst)) . (parseSingleToken expectInt)

parseVar :: Parser Expression
parseVar = (fmap (first ExVar)) . (parseSingleToken expectIdent)

parseSubexpression ts0 = do
  (_, ts1) <- parseSingleToken expectOpenPar ts0
  (e, ts2) <- parseSum ts1
  (_, ts3) <- parseSingleToken expectClosePar ts2
  return (e, ts3)

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe p m@(Just x)
  | p x       = m
  | otherwise = Nothing

parse :: [Token] -> Maybe Expression
parse = fmap (fst) . filterMaybe (null . snd) . parseSum

unOpSemantics :: Map UnOp (Float -> Float)
unOpSemantics = fromList [
    (UnOpNegate, negate),
    (UnOpAbs, abs),
    (UnOpLn, log),
    (UnOpExp, exp)
  ]

binOpSemantics :: Map BinOp (Float -> Float -> Float)
binOpSemantics = fromList [
    (BinOpAdd, (+)),
    (BinOpSub, (-)),
    (BinOpMul, (*)),
    (BinOpDiv, (/)),
    (BinOpMod, mod'),
    (BinOpPow, (**)),
    (BinOpRoot, (\a b -> a ** (1 / b)))
    ]

eval :: Expression -> Map String Float -> Maybe Float
eval (ExConst x) _ = Just x
eval (ExVar v) m = M.lookup v m
eval (ExUnary op ex) m = (M.lookup op unOpSemantics) <*> (eval ex m)
eval (ExBinary op ex1 ex2) m = (M.lookup op binOpSemantics) <*> (eval ex1 m) <*> (eval ex2 m)

data RpnCommand = RpnConst Float
               | RpnUnOp  UnOp
               | RpnBinOp BinOp
               deriving (Show)
 
jmp :: Int -> Int -> Int -> Int
jmp ptr n upperBound
    | newPtr < 0 || newPtr > upperBound = throw (IndexOutOfBoundsException newPtr) 
    | otherwise = newPtr
  where newPtr = ptr + n
  
jmpIf :: Int -> Int -> Float -> Int -> Int
jmpIf ptr n top upperBound
    | top == 0 = jmp ptr n upperBound
    | otherwise = ptr + 1

execRpn :: RpnCommand -> Int -> [Float] -> Int -> ([Float], Int)
execRpn (RpnConst x) ptr zs _ = (x:zs, ptr + 1)
execRpn (RpnUnOp op) ptr (z:zs) upperBound
    | op == UnOpJmp = (zs, jmp ptr (round z) upperBound)
    | otherwise = (((unOpSemantics ! op) z): zs, ptr + 1)
execRpn (RpnBinOp op) ptr (u:(v:zs)) upperBound
    | op == BinOpJmpIf = (zs, jmpIf ptr (round v) u upperBound)
    | otherwise = (((binOpSemantics ! op) u v) : zs, ptr + 1) 
   
runRpn :: [RpnCommand] -> [Float]
runRpn commands = runRpnHelper commands 0 []

runRpnHelper:: [RpnCommand] -> Int -> [Float] -> [Float]
runRpnHelper commands ptr stack 
    | ptr == cLength = stack
    | otherwise = runRpnHelper commands newPtr newStack 
  where cLength = length commands
        (newStack, newPtr) = execRpn (commands !! ptr) ptr stack (cLength - 1)

expressionToRpn :: Expression -> [RpnCommand]
expressionToRpn (ExConst x) = [RpnConst x]
expressionToRpn (ExUnary op ex) = (expressionToRpn ex) ++ [RpnUnOp op]
expressionToRpn (ExBinary op ex1 ex2) = (expressionToRpn ex1) ++ (expressionToRpn ex2) ++ [RpnBinOp op]

--test jmp: runRpn [RpnConst 7, RpnConst 1, RpnConst 5,  RpnConst 3, RpnBinOp BinOpSub, RpnUnOp UnOpJmp, RpnConst 3, RpnConst 11, RpnBinOp BinOpAdd]
--test jmpIf: runRpn [RpnConst 7, RpnConst 1, RpnConst 5,  RpnConst 3, RpnBinOp BinOpSub, RpnBinOp BinOpJmpIf,RpnConst 3,RpnBinOp BinOpAdd]
--test jmpIf: runRpn [RpnConst 7, RpnConst 2, RpnConst 5,  RpnConst 5, RpnBinOp BinOpSub, RpnBinOp BinOpJmpIf, RpnConst 3, RpnConst 11, RpnBinOp BinOpAdd]