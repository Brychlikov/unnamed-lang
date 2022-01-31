module Ast.Lower where

import qualified Ast.Full as F
import Ast.Common
import Ast.Normal
import Data.List (partition)
import Control.Comonad.Cofree

lower :: F.Expr -> Expr
lower (F.Const l) = econst l
lower (F.Var name) = evar name
lower (F.Call e1 e2) = ecall (lower e1) (lower e2)
lower (F.Binop op e1 e2) = ecall (ecall (evar $ op_to_name op) (lower e1)) (lower e2)
    where
    op_to_name Plus  = "(+)"
    op_to_name Minus = "(-)"
    op_to_name Mult  = "(*)"
    op_to_name Div   = "(/)"
    op_to_name Pair  = "(,)"
    op_to_name EqEq  = "(==)"
    op_to_name Neq   = "(!=)"

lower (F.Neg e) = ecall (evar "negate") (lower e)

lower (F.Let (F.Simple pat e1) e2) = elet pat (lower e1) (lower e2)

lower (F.Let (F.FunBinding name pats e1) e2) =
    -- elet (PVar name) 
    --     (elfix (elambda (PVar name) 
    --            (lower $ F.Lambda pats e1)))
    --     (lower e2)
    elet (PVar name) (elfix name (lower $ F.Lambda pats e1)) (lower e2)

-- lower (F.Let (F.FunBinding name [pat] e1) e2) = elet (FunBinding  name pat (lower e1)) (lower e2)
-- lower (F.Let (F.FunBinding name (p:pats) e1) e2) = 
--     elet (FunBinding name p (lower $ F.Lambda pats e1)) (lower e2)

lower (F.Lambda pats body) = lambdaFlatten pats (lower body)

lower (F.Cond e1 e2 e3) = econd (lower e1) (lower e2) (lower e3)

lambdaFlatten :: [Pattern] -> Expr -> Expr
lambdaFlatten [pat] body = elambda pat body
lambdaFlatten (p:pats) body = elambda p (lambdaFlatten pats body)
lambdaFlatten [] _ = undefined

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith pred = inner ([], []) where
    inner (l, r) [] = (reverse l, reverse r)
    inner (l, r) (x:xs) = case pred x of
        Left el  -> inner (el:l, r) xs
        Right el -> inner (l, el:r) xs

lowerProg :: F.Prog -> Prog
lowerProg (F.Prog decls) = Prog datas lets where
    (datas, lets) = partitionWith pred decls
    pred (F.LDecl (F.Simple pat e)) = Right (Simple pat (lower e))
    pred (F.LDecl (F.FunBinding name pats e)) =
         Right $ Simple (PVar name) ((lambdaFlatten pats . lower) e)
    pred (F.DDecl d) = Left d