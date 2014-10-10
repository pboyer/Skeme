import Prelude hiding (lookup)
import Data.Map hiding (foldr)
 
instance Show (a -> b) where
    show _ = "VFunc"

data SVal = 
    VNum Double
    | VFunc ([SVal] -> SVal)
    | VLambda [String] SExpr
    deriving Show

data SExpr = 
    SAtom SVal
    | SId String
    | SList [SExpr]
    deriving Show 

data SEnv = 
    EmptyEnv 
    | Env (Map String SVal) SEnv
    deriving (Show)

find :: String -> SEnv -> SVal
find _ EmptyEnv = error "Unbound identifier"
find s (Env m e) = case (lookup s m) of
    Nothing -> (find s e)
    (Just v) -> v

eval :: SEnv -> SExpr -> SVal
eval _ (SAtom v) = v
eval e (SId i) = find i e
eval e (SList xs) = case (fmap (eval e) xs) of
    ((VFunc f):vs) -> f vs
    ((VLambda ids x):vs) -> apply e ids x vs 
    _ -> (error "Unexpected input" )

apply :: SEnv -> [String] -> SExpr -> [SVal] -> SVal
apply env ids exp args = eval nenv exp 
    where nenv = Env (fromList $ zip ids args) env

wrapBinary :: (Double -> Double -> Double) -> SVal
wrapBinary f = VFunc (\(v:vs) -> (VNum (foldr f (unwrapNum v) (fmap unwrapNum vs))))

unwrapNum :: SVal -> Double
unwrapNum (VNum v) = v
unwrapNum _ = error "Unexpected type encountered"

defaultEnv = Env (fromList [("+", wrapBinary (+))]) EmptyEnv

plus22 = SList [(SId "+"), (SAtom (VNum 2)), (SAtom (VNum 2))]

lamplus21 = (SList
    [(SAtom (VLambda ["a","b"] (SList [(SId "+"),(SId "a"),(SId "b")]))), 
    (SAtom (VNum 2)), 
    (SAtom (VNum 1))])

