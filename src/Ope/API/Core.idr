||| API.Core模块定义了类型安全的API表示系统
||| 提供用于构建路由和处理HTTP请求的核心类型和函数
module Ope.API.Core
import public Ope.API.Operator

%default total

namespace Path
  ||| Path 表示 API 路径的类型安全描述
  ||| 通过组合 Path 构造器，可以构建出完整的路由路径
  public export
  data Path : Type where
    ||| 静态路径段，例如 "users", "products" 等
    StaticPath : String -> Path
    
    ||| 路径组合操作符，用于连接两个路径
    ||| 例如: StaticPath "api" :> StaticPath "users"
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
  ||| 例如: StaticPath "users" :> Get (List User)
  (:>) : Show resp => Path -> Endpoint resp -> API


||| EndpointResult 是类型函数，计算终结点的响应类型
||| 它从 Endpoint 类型中提取出响应类型信息
public export
EndpointResult : Endpoint resp -> Type
EndpointResult (Get respType) = respType  -- GET 方法返回指定的响应类型

||| HandlerType 计算处理特定 API 所需的处理器函数类型
||| 对于简单 API，处理器类型就是返回类型
||| 对于带参数的 API，处理器类型会包含参数类型
||| 例如: API "users/:id" 的 HandlerType 将是 (id -> User)
public export
HandlerType : API -> Type
HandlerType (path :> endpoint) = EndpointResult endpoint
-- 这个版本忽略了路径参数，完整版本应该是:
-- HandlerType (path :> endpoint) = PathParam path (EndpointResult endpoint)


||| 路由记录类型
||| 将 API 定义与对应的处理器函数关联起来
public export
record Route where
  constructor MkRoute
  ||| API 定义，描述路径和端点
  api: API
  ||| 处理器函数，类型由 API 定义决定
  handler : HandlerType api

||| 服务器数据类型
||| 包含一组路由定义，用于处理 HTTP 请求
public export
data Server : Type where
  ||| 创建服务器实例，包含路由列表
  MkServer : (routes : List Route) -> Server

