module Ope.WAI.Request

import Ope.WAI.Core
import Data.SortedMap

import Derive.Prelude

%language ElabReflection

||| HTTP方法枚举
||| 定义了支持的HTTP请求方法
public export
data Method = GET | POST | HEAD

%runElab derive "Method" [Show,Eq,Ord]


||| 解析HTTP方法字符串
||| 
||| 将字符串转换为Method枚举值
||| @ s 方法字符串("GET", "POST"等)
public export
method : String -> Either HTTPErr Method
method "GET"  = Right GET
method "POST" = Right POST
method "HEAD" = Right HEAD
method _      = Left InvalidRequest


||| HTTP请求记录类型
||| 包含HTTP请求的所有相关信息
public export
record Request where
  constructor MkRequest
  ||| HTTP请求方法(GET, POST等)
  method  : Method
  ||| 请求的URI路径
  uri     : String
  ||| 查询参数映射
  queryParams : SortedMap String String
  ||| HTTP版本
  version : Version
  ||| 请求头部字段映射
  headers : Headers
  ||| 请求体长度
  length  : Nat
  ||| 请求体内容类型
  type    : Maybe String
  ||| 请求体字节流
  body    : HTTPStream ByteString

