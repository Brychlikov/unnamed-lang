module Ast.Lower where

import qualified Ast.Full as F
import Ast.Common
import Ast.Normal

lower :: F.Expr -> Expr
lower (F.Const l) = Const l
lower (F.Var name) = Var name
lower (F.Call e1 e2) = Call (lower e1) (lower e2)
lower (F.Binop op e1 e2) = Call (Call (Var $ op_to_name op) (lower e1)) (lower e2)
    where 
    op_to_name Plus  = "(+)"
    op_to_name Minus = "(-)"
    op_to_name Mult  = "(*)"
    op_to_name Div   = "(/)"
lower (F.Neg e) = Call (Var "negate") (lower e)
lower _ = undefined
