{
module Parser where
import Data.Char
}

%name parser
%tokentype { Token }
%error { parseError }


%token 
  true		{ TokenTrue   }
  false     { TokenFalse  }
  num       { TokenNum $$ }						
  '+'       { TokenPlus   }
  '&'       { TokenAnd    }												
%%


Exp	: true 		{ BTrue }
		| 		false { BFalse }

Exp1: num   { Num $1 }

Exp2:  			Exp1 '+' Exp1 { Add $1 $3 }
		| 		Exp '&' Exp { And $1 $3 }		


{
parseError :: [Token] -> a
parseError _ = error "Syntax error: sequência de caracteres inválida!"

--data Exp = BTrue
--		|  BFalse
--		|  Num Int
--		|  Add Exp Exp
--		|  And Exp Exp		

data Token = TokenTrue
		|		TokenFalse
		|		TokenNum Int
		|		TokenPlus
		|		TokenAnd



lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexBool (c:cs)
     | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer _ = error "Lexical error: caracter inválido!"


-- Lê um token booleano
lexBool cs = case span isAlpha cs of
               ("true", rest) -> TokenTrue : lexer rest
               ("false", rest) -> TokenFalse : lexer rest


-- Lê um token numérico 
lexNum cs = case span isDigit cs of
              (num, rest) -> TokenNum (read num) : lexer rest


lexPlus cs = case span isAlpha cs of
               ("+", rest) -> TokenPlus : lexer rest

lexAnd cs = case span isAlpha cs of
               ("&", rest) -> TokenAnd : lexer rest

}
