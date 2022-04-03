{
module Parser where
import Data.Char
}

%name parser
%tokentype { Token }
%error { parseError }


%token 
  true	    { TokenTrue   }
  false     { TokenFalse  }
  num       { TokenNum $$ }
  fst       { TokenFst    }	
  snd       { TokenSnd    }	
  if 	    { TokenIf     }
  else	    { TokenElse   }						
  '+'       { TokenPlus   }
  '&'       { TokenAnd    }	
  ','       { TokenComma  }
  '('       { TokenOB     }
  ')'       { TokenCB     }	
  '{'       { TokenOK     }
  '}'       { TokenCK     }											
%%


Exp	: true 		                                { BTrue }
        | false                                         { BFalse }
        | num                                           { Num $1 }
        | Exp '+' Exp                                   { Add $1 $3 }
	| Exp '&' Exp                                   { And $1 $3 }		
        | '(' Exp ',' Exp ')'                           { NewPair $2 $4 }
        | fst Exp                                       { FirstPair $2 } --ler recursivo e second
        | snd Exp                                       { LastPair $2 } --ler recursivo e second
        | if '(' Exp ')' '{' Exp '}' else '{' Exp '}'	{ If $3 $6 $10 }


{
parseError :: [Token] -> a
parseError _ = error "Syntax error: sequência de caracteres inválida!"
	

data Token = TokenTrue
	|   TokenFalse
	|   TokenNum Int
	|   TokenPlus
	|   TokenAnd
        |   TokenOB
        |   TokenCB
        |   TokenOK
        |   TokenCK
        |   TokenComma
        |   TokenFst
        |   TokenSnd
        |   TokenIf
        |   TokenElse


-- Árvore de sintaxe abstrata
data Expr = BTrue 
    | BFalse
    | Num Int
    | NewPair Expr Expr -- CONSTRUTORES
    | FirstPair Expr    -- CONSTRUTORES
    | LastPair Expr     -- CONSTRUTORES
    | Add Expr Expr
    | And Expr Expr
    | If Expr Expr Expr
    deriving (Show, Eq)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexKeyWord (c:cs)
     | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('{':cs) = TokenOK : lexer cs
lexer ('}':cs) = TokenCK : lexer cs
lexer _ = error "Lexical error: caracter inválido!"

-- Lê um token numérico 
lexNum cs = case span isDigit cs of
              (num, rest) -> TokenNum (read num) : lexer rest

lexPlus cs = case span isAlpha cs of
               ("+", rest) -> TokenPlus : lexer rest

lexAnd cs = case span isAlpha cs of
               ("&", rest) -> TokenAnd : lexer rest

-- Lê um token de palavra reservada
lexKeyWord cs = case span isAlpha cs of
    ("true", rest)  -> TokenTrue : lexer rest
    ("false", rest) -> TokenFalse : lexer rest
    ("fst", rest)	  -> TokenFst : lexer rest
    ("snd", rest)	  -> TokenSnd : lexer rest
    ("if", rest)	  -> TokenIf : lexer rest
    ("else", rest)  -> TokenElse : lexer rest
}
