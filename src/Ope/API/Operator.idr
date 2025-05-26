||| Define operator precedences for the API DSL
module Ope.API.Operator

-- Operator :/ is used to define a route with a handler function
public export
infixr 6 :/

-- Operator :-> is used to define a route with a handler function
public export
infixr 5 :->

-- Operator :<|> is used to define a route with a handler function
public export
infixr 4 :<|>

-- Operator :=> is used to define a route with a handler function
public export
infixr 5 :=>