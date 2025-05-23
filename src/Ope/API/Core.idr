||| API.Core模块定义了类型安全的API表示系统
||| 提供用于构建路由和处理HTTP请求的核心类型和函数
module Ope.API.Core
import public Ope.API.Operator
import Data.Vect

%default total

public export
data HList : (ts : List Type) -> Type where
  Nil  : HList Nil
  (::) : (v : t) -> (vs : HList ts) -> HList (t :: ts)

namespace Path
  ||| Path 表示 API 路径的类型安全描述
  ||| 通过组合 Path 构造器，可以构建出完整的路由路径
  public export
  data Path : Type where
    ||| 静态路径段，例如 "users", "products" 等
    Static : String -> Path

    ||| 捕获路径段，例如 ":id"
    Capture : String -> Type -> Path
    
    ||| 路径组合操作符，用于连接两个路径
    ||| 例如: Static "api" :> Static "users"
    (:>) : Path -> Path -> Path  -- 组合路径

  ||| Endpoint 表示 API 终结点，包含 HTTP 方法和响应类型
  ||| 参数化类型 resp 用于携带响应类型信息
  public export
  data Endpoint : Type -> Type where
    ||| GET 请求终结点，resp 指定返回类型
    ||| 例如: Get String, Get User, Get (List Product)
    Get : (resp : Type) -> Endpoint resp
    -- 可以扩展添加 Post, Put, Delete 等其他 HTTP 方法

||| API 类型将路径和终结点组合成完整的 API 描述
||| 它是类型安全 API 框架的核心
public export
data API : Type where
  ||| 连接路径和终结点，形成完整 API
  ||| Show resp 约束确保响应类型可以被序列化
  ||| 例如: Static "users" :> Get (List User)
  (:>) : Show resp => Path -> Endpoint resp -> API


||| EndpointResult 是类型函数，计算终结点的响应类型
||| 它从 Endpoint 类型中提取出响应类型信息
public export
EndpointResult : Endpoint resp -> Type
EndpointResult (Get respType) = respType  -- GET 方法返回指定的响应类型

public export 
GetEndpointResultType : API -> Type
GetEndpointResultType (_ :> (Get res)) = res

||| 捕获路径段类型
||| 用于表示捕获路径段中的参数类型
public export
HandlerPathParam : Path -> Type
HandlerPathParam (Capture _ paramType) = paramType
HandlerPathParam (Static _) = ()
HandlerPathParam (path :> rest) = case (HandlerPathParam path, HandlerPathParam rest) of
  ((), paramType) => paramType
  (paramType, ()) => paramType
  (paramType1, paramType2) => paramType1 -> paramType2

||| 获取路径的捕获类型
||| 例如: PathCaptureTypes (Static "users" :> Capture "id" Nat :> Capture "name" String) [] = [Nat, String]
public export
PathCaptureTypes : Path -> List Type -> List Type
PathCaptureTypes (Capture _ paramType) types = paramType :: types
PathCaptureTypes (Static _) types = types
PathCaptureTypes (rest :> path) types = (PathCaptureTypes rest types) ++ (PathCaptureTypes path types)

public export
APICaptureTypes : API -> List Type
APICaptureTypes (path :> endpoint) = PathCaptureTypes path []

public export
HandlerType' : (pathTypes : List Type) -> (resType : Type)  -> Type
HandlerType' [] resType = resType
HandlerType' (t :: ts) resType = t -> (HandlerType' ts resType)

public export
APIHandlerType' : API -> Type
APIHandlerType' (path :> (Get res)) = HandlerType' (PathCaptureTypes path []) res

||| 路由记录类型
||| 将 API 定义与对应的处理器函数关联起来
public export
record Route where
  constructor MkRoute
  ||| API 定义，描述路径和端点
  api: API
  ||| 处理器函数，类型由 API 定义决定
  handler : APIHandlerType' api

||| 服务器数据类型
||| 包含一组路由定义，用于处理 HTTP 请求
public export
data Server : Type where
  ||| 创建服务器实例，包含路由列表
  MkServer : (routes : List Route) -> Server

