||| The Operator module implements the core functionality of the API operator
||| Provides a mechanism to handle different API operators
module Ope.API.Operator

-- 操作符 :> 优先级定义
public export
infixr 6 :>

-- 操作符 :-> 优先级定义
public export
infixr 5 :->

-- 操作符 :<|> 优先级定义
public export
infixr 4 :<|>

public export
infixr 5 :=>