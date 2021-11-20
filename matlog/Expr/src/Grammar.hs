module Grammar where

data BinaryOp = Implies | Or | And
data UnaryOp = Not

instance Show BinaryOp where
    show Implies = "->"
    show Or      = "|"
    show And     = "&"
instance Show UnaryOp where
    show Not = "!"

data Expr = Binary BinaryOp Expr Expr
          | Unary UnaryOp Expr
          | Var String

instance Show Expr where
    show (Binary op lhs rhs) = "(" ++ show op ++ "," ++ show lhs ++ "," ++ show rhs ++ ")"
    show (Unary op arg)      = "(" ++ show op ++ show arg ++ ")"
    show (Var name)          = name
