module Grammar where

data SideEffect = SideEffect String Integer
instance Show SideEffect where
    show (SideEffect name val) = name ++ " = " ++ show val
