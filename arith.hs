import Control.Exception
import Data.Typeable

newtype NoRuleApplies = NoRuleApplies String
        deriving (Show, Typeable)

instance Exception NoRuleApplies

data Term =
    TmTrue 
    | TmFalse 
    | TmIf  Term Term Term
    | TmZero 
    | TmSucc  Term
    | TmPred  Term
    | TmIsZero  Term
    deriving (Eq, Show)

isnumericval :: Term -> Bool
isnumericval t = case t of
    TmZero -> True
    TmSucc t1 -> isnumericval t1 
    _ -> False

isval :: Term -> Bool
isval t
    | t == TmTrue = True
    | t == TmFalse = True
    | isnumericval t = True
    | otherwise = False
-- isval t = case t of
--     TmTrue -> True
--     TmFalse -> True
--     _ -> isnumericval t 

eval1 :: Term -> Term
eval1 t = case t of
    TmIf TmTrue t2 t3 -> t2 
    TmIf TmFalse t2 t3 -> t3 
    TmIf t1 t2 t3 -> let t1' = eval1 t1 in TmIf t1' t2 t3
    TmSucc t1 -> let t1' = eval1 t1 in TmSucc t1'
    TmPred TmZero -> TmZero
    TmPred (TmSucc nv1) | isnumericval nv1 -> nv1
    TmPred t1 -> let t1' = eval1 t1 in TmPred t1'
    TmIsZero TmZero -> TmTrue
    TmIsZero (TmSucc nv1) | isnumericval nv1 -> TmFalse
    TmIsZero t1 -> let t1' = eval1 t1 in TmIsZero t1'
    _ -> throw (NoRuleApplies "No rule applies")

eval :: Term -> IO ()
eval t = do
    result <- try (evaluate  (eval1 t)) :: IO (Either NoRuleApplies Term)
    case result of
        Left ex -> putStrLn $ "Caught exception: " ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val

evalAll :: Term -> Term
evalAll t 
    | isval t = t 
    | otherwise = evalAll (eval2 t)

eval2 :: Term -> Term
eval2 t = case t of 
    TmIf TmTrue t2 t3 -> t2 
    TmIf TmFalse t2 t3 -> t3 
    TmIf t1 t2 t3 ->
        case eval2 t1 of 
            t1' -> TmIf t1' t2 t3 
    TmSucc t1 -> TmSucc (eval2 t1)
    TmPred TmZero -> TmZero
    TmPred (TmSucc nv1) | isnumericval nv1 -> nv1 
    TmPred t1 -> TmPred (eval2 t1) 
    TmIsZero TmZero -> TmTrue 
    TmIsZero (TmSucc nv1) | isnumericval nv1 -> TmFalse 
    TmIsZero t1 -> TmIsZero (eval2 t1) 
    _ -> t

bigStep :: Term -> Term
bigStep t = case t of
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmZero -> TmZero 
    TmSucc t1 -> TmSucc (bigStep t1)
    TmIf t1 t2 t3 ->
        case bigStep t1 of 
            TmTrue -> bigStep t2 
            TmFalse -> bigStep t3 
            _ -> throw (NoRuleApplies "if condition guard must be boolean")
    TmPred t1 ->
        case bigStep t1 of 
            TmZero -> TmZero
            TmSucc nv1 | isnumericval nv1 -> nv1
            _ -> throw (NoRuleApplies "Pred needs numeric value")
    TmIsZero t1 ->
        case bigStep t1 of 
            TmZero -> TmTrue
            TmSucc nv1 | isnumericval nv1 -> TmFalse 
            _ -> throw (NoRuleApplies "IsZero needs numeric value")


test1 = evalAll (TmIf TmTrue TmZero (TmSucc TmZero))
test2 = evalAll (TmPred (TmSucc TmZero))
test3 = evalAll (TmIsZero TmZero)

test4 = bigStep (TmIf TmTrue TmZero (TmSucc TmZero))
test5 = bigStep (TmPred (TmSucc TmZero))
test6 = bigStep (TmIsZero TmZero)