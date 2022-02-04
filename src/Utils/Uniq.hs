{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Utils.Uniq where 
import Control.Monad.State (StateT, MonadTrans (lift), get, put)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Identity (Identity)

newtype UniqT m a = UniqT { runUniqT :: StateT (Map.Map Text Int) m a}
    deriving (Functor, Applicative, Monad, MonadTrans)

class Monad m => MonadUnique m where 
    unique :: Text -> m Text

instance Monad m => MonadUnique (UniqT m) where 
    unique t = do 
        st <- UniqT get
        case Map.lookup t st of 
            Just cnt -> do
                UniqT (put $ Map.insert t (cnt + 1) st)
                return $ t `T.append` T.pack (show cnt)
            Nothing -> do 
                UniqT $ put $ Map.insert t 1 st
                return t

type Uniq = UniqT Identity
