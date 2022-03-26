{

-- Detalhes dessa implementação podem ser encontrados em:
-- https://www.haskell.org/happy/doc/html/sec-using.html

module Parser where
import Data.Char

}

%name parser
%tokentype { Token }
%error { parseError }

%token 
  true		{ TokenTrue }
  -- Fazer para outras constantes (False)
  num		{ TokenNum $$ }
  '+'		{ TokenPlus }
  -- Adicionar outros operadores (AND, OR e MULT)

%%

Exp	: true 		{ BTrue }    
        | num		{ Num $1 }
        | Exp '+' Exp	{ Add $1 $3 }
	-- Adicionar para as outras constantes e operadores


-- Inicio da codificação do Lexer
---------------------------------
{

parseError :: [Token] -> a
parseError _ = error "Syntax error: sequência de caracteres inválida!"

-- Árvore de sintaxe abstrata (desenvolvida em aula) 
-- Adicionar outros operadores (Mult e Or)

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          | If Expr Expr Expr
          deriving (Show, Eq)

-- Tokens permitidos na linguagem (adicionar outras constantes e operadores)

data Token = TokenTrue
           | TokenNum Int
           | TokenPlus
           deriving Show

-- Analisador léxico (lê o código e converte em uma lista de tokens)
-- Adicionar ao final para os outros operadores
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexBool (c:cs)
     | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer _ = error "Lexical error: caracter inválido!"

-- Lê um token booleano
-- Adicionar a constante false ao final
lexBool cs = case span isAlpha cs of
               ("true", rest) -> TokenTrue : lexer rest

-- Lê um token numérico 
lexNum cs = case span isDigit cs of
              (num, rest) -> TokenNum (read num) : lexer rest

}
