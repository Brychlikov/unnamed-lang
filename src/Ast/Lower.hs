module Ast.Lower where

import qualified Ast.Full as F
import Ast.Common
import Ast.Normal

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


lower (F.Lambda pats body) = helper pats (lower body) where 
    helper :: [Pattern] -> Expr -> Expr
    helper [pat] body = elambda pat body
    helper (p:pats) body = elambda p (helper pats body)
    helper [] _ = undefined

lower (F.Cond e1 e2 e3) = econd (lower e1) (lower e2) (lower e3)
