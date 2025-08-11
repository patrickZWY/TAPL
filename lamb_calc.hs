
import Text.Printf (printf)
import Control.Exception
import Data.Typeable

newtype NoRuleApplies = NoRuleApplies String
        deriving (Show, Typeable)

instance Exception NoRuleApplies

data Term
    = TmVar Integer Integer
    | TmAbs String Term
    | TmApp Term Term
    deriving (Eq, Show)

data Binding = NameBind deriving (Eq, Show)

type Context = [(String, Binding)]

ctxlength :: Context -> Int
ctxlength = length

index2name :: Context -> Integer -> String
index2name ctx x
    | x' >= 0 && x' < ctxlength ctx = fst (ctx !! x')
    | otherwise = error (printf "Variable lookup failure: offset: %d, ctx size: %d"
                                x (length ctx))
    where
        x' = fromIntegral x :: Int

name2index :: Context -> String -> Integer
name2index ctx x =
    case ctx of
        [] -> error ("Identifier " ++ x ++ " is unbound")
        (y, _) : rest ->
            if y == x then 0
            else 1 + name2index rest x

pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx x = ((x', NameBind) : ctx, x')
    where
        x' = fresh x
        fresh y
            | any (\pair -> fst pair == y) ctx = fresh (y ++ "'")
            | otherwise              = y

printTm :: Context -> Term -> String
printTm ctx (TmAbs x t1) =
    "(lambda " ++ x' ++ ". " ++ printTm ctx' t1 ++ ")"
        where (ctx', x') = pickfreshname ctx x
printTm ctx (TmApp t1 t2) =
    "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
printTm ctx (TmVar x n)
    | ctxlength ctx == fromIntegral n = index2name ctx x
    | otherwise                       = "[bad index]"


isnamebound :: Context -> String -> Bool
isnamebound ctx x =
    case ctx of
        [] -> False 
        (y, _) : rest ->
            if y == x then True
            else isnamebound rest x

termShift :: Integer -> Term -> Term
termShift d t =
    walk 0 t
        where
            walk c t' = 
                case t' of
                    TmVar x n -> if x >= c then TmVar (x + d) (n + d)
                                            else TmVar x (n + d)
                    TmAbs x t1 -> TmAbs x (walk (c + 1) t1)
                    TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

termSubst :: Integer -> Term -> Term -> Term
termSubst j s t =
    walk 0 t
        where
            walk c t' = case t' of 
                TmVar x n -> if (x == (j + c)) then termShift c s else TmVar x n
                TmAbs x t1 -> TmAbs x (walk (c + 1) t1)
                TmApp  t1 t2 -> TmApp (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

-- evaluation
isval :: Context -> Term -> Bool 
isval _ (TmAbs _ _) = True 
isval _ _  = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
    TmApp (TmAbs _ t12) v2 | isval ctx v2 -> Just (termSubstTop v2 t12)
    TmApp v1 t2 | isval ctx v1 -> TmApp v1 <$> eval1 ctx t2
    TmApp t1 t2 -> (\t1' -> TmApp t1' t2) <$> eval1 ctx t1
    _ -> Nothing

evalAll :: Context -> Term -> Term
evalAll ctx t = case eval1 ctx t of 
    Nothing -> t 
    Just t' -> evalAll ctx t'

eval2 :: Context -> Term -> Term
eval2 ctx t = case t of
    TmApp (TmAbs _ t12) v2 | isval ctx v2 -> termSubstTop v2 t12
    TmApp v1 t2 | isval ctx v1 -> TmApp v1 t2' where t2' = eval2 ctx t2
    TmApp t1 t2 -> TmApp t1' t2 where t1' = eval2 ctx t1
    _ -> throw (NoRuleApplies "No Rule Applies") 

evalAll2 :: Context -> Term -> IO Term
evalAll2 ctx t = do 
    r <- try (evaluate (eval2 ctx t)) :: IO (Either NoRuleApplies Term)
    case r of 
        Left _ -> pure t 
        Right t' -> evalAll2 ctx t'

isVal2 :: Term -> Bool
isVal2 (TmAbs _ _) = True 
isVal2 _ = False

evalBig :: Term -> Term
evalBig t | isVal2 t = t
evalBig (TmApp t1 t2) = 
    case evalBig t1 of 
        TmAbs _ t12 ->
            evalBig (termSubstTop v2 t12) where v2 = evalBig t2 
        _ -> throw (NoRuleApplies "function pos did not evaluate to a lambda")
evalBig (TmVar _ _) = throw (NoRuleApplies "free var in evaluation pos")
evalBig _ = throw (NoRuleApplies "no rule applies")
