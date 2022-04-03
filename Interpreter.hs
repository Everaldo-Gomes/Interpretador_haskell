module Interpreter where
import Parser 
import Language.Haskell.TH (Exp)

data Ty = TNum 
    | TBool 
    | TPair Ty Ty  -- NOVO TIPO
    deriving Show

-- Função que avalia um passo de execução
step :: Expr -> Maybe  Expr
step (Add (Num n1) (Num n2)) = Just (Num (n1 + n2))
step (Add (Num n1) e2) = case (step e2) of
                            Just e2' -> Just (Add (Num n1) e2')
                            Nothing  -> Nothing
                            
step (Add e1 e2) = case (step e1) of
                        Just e1' -> Just (Add e1' e2)
                        Nothing  -> Nothing
                        
step (And BTrue e2) = Just e2
step (And BFalse _) = Just BFalse
step (And e1 e2) = case (step e1) of
                        Just e1' -> Just (And e1' e2)
                        Nothing  -> Nothing

step (If BTrue e1 _) = step e1
step (If BFalse _ e2) = step e2
step (If e1 e2 e3) = case (step e1) of
                       Just e' -> Just (If e' e2 e3)
                       Nothing -> Nothing

step (NewPair e1 e2 ) = case (step e1) of
                        Just e' -> case (step e2) of 
                                    Just e2'-> Just (NewPair e' e2')
                                    Nothing -> Nothing
                        Nothing -> Nothing

step (FirstPair (NewPair e1 e2)) = Just e1
step (LastPair (NewPair e1 e2)) = Just e2
step e = Just e

-- Função que avalia uma expressão até apresentar um resultado ou gerar um erro
eval :: Expr -> Maybe Expr
eval e = case (step e) of 
           Just e' -> if (e == e') then
                        Just e
                      else
                        eval e'
           _ -> error "Semantic error: erro avaliando expressão!" 
  

-- Função que verifica o tipo de uma expressão
typeof :: Expr -> Maybe Ty
typeof BTrue = Just TBool 
typeof BFalse = Just TBool 
typeof (Num _) = Just  TNum

typeof (NewPair e1 e2) = case (typeof e1) of
                        Just t1 -> case (typeof e2) of
                                        Just t2 -> Just (TPair t1 t2)
                                        _         -> Nothing 
                        _         -> Nothing


typeof (Add e1 e2) = case (typeof e1) of
                        Just TNum -> case (typeof e2) of
                                        Just TNum -> Just TNum 
                                        _         -> Nothing 
                        _         -> Nothing

                        
typeof (And e1 e2) = case (typeof e1, typeof e2) of
                        (Just TBool, Just TBool) -> Just TBool 
                        _                        -> Nothing

typeof (If e e1 e2) = 
    case (typeof e) of
        Just TBool -> case (typeof e1, typeof e2) of
                        (Just TBool, Just TBool) -> Just TBool
                        (Just TNum, Just TNum) -> Just TNum 
                        _                      -> Nothing 
        _          -> Nothing 
                        

typeof (FirstPair (NewPair e1 e2)) =
  case (typeof e1, typeof e2) of
    (Just TNum, _)  -> Just TNum
    (Just TBool, _) -> Just TBool
    _                -> Nothing

typeof (LastPair (NewPair e1 e2)) =
  case (typeof e1, typeof e2) of
    (_, Just TNum)  -> Just TNum
    (_, Just TBool) -> Just TBool
    _               -> Nothing
  
  
-- Função que faz a verificação de tipos
typecheck :: Expr -> Expr
typecheck e = case (typeof e) of 
                Just _ -> e
                _ -> error "Type error: erro na verificação de tipos!"


-- Ler os códigos e chamar o interpretador
main = getContents >>= print . eval . typecheck . parser . lexer 