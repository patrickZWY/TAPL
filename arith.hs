import Control.Exception
import Data.Typeable
import GHC.Base (IO(IO))

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