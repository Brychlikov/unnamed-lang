module Types where 


newtype TVar = TV String 
    deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

data Type 
    = TNum 
    | TBool 
    | TString 
    | TVar TVar
    | TScheme Scheme
    | TArr Type Type
    deriving (Show, Eq, Ord)
