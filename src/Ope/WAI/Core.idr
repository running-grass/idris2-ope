||| WAI.Core模块定义了Web应用接口的核心类型
||| 包括HTTP错误、方法、版本和响应类型
module Ope.WAI.Core

import Derive.Prelude
import Data.SortedMap

import FS.Posix
import FS.Socket

import IO.Async.Loop.Posix
import IO.Async.Loop.Epoll
import public Data.ByteVect

%language ElabReflection

||| HTTP错误类型
||| 表示HTTP处理过程中可能发生的错误
public export
data HTTPErr : Type where
  ||| 请求头部大小超过限制
  HeaderSizeExceeded  : HTTPErr
  ||| 请求体大小超过限制
  ContentSizeExceeded : HTTPErr
  ||| 请求格式无效
  InvalidRequest      : HTTPErr

%runElab derive "HTTPErr" [Show,Eq,Ord]

||| HTTPErr的字符串插值实现
export
Interpolation HTTPErr where
  interpolate HeaderSizeExceeded  = "header size exceeded"
  interpolate ContentSizeExceeded = "content size exceeded"
  interpolate InvalidRequest      = "invalid HTTP request"

||| HTTP拉取流类型
||| 表示可以从中拉取HTTP数据的异步流
||| @ o 输出类型
||| @ r 结果类型
public export
0 HTTPPull : Type -> Type -> Type
HTTPPull o r = AsyncPull Poll o [Errno,HTTPErr] r

||| HTTP字节流类型
||| 表示可以从中拉取字节数据的HTTP流
||| @ o 输出类型
public export
0 HTTPStream : Type -> Type
HTTPStream o = AsyncPull Poll o [Errno,HTTPErr] ()

||| HTTP头部类型
||| 表示为键值对映射，键和值都是字符串
public export
0 Headers : Type
Headers = SortedMap String String
||| HTTP版本枚举
||| 定义了支持的HTTP协议版本
public export
data Version = V10 | V11 | V20

%default total
%default covering
%runElab derive "Version" [Show,Eq,Ord]
||| 解析HTTP版本字符串
||| 
||| 将字符串转换为Version枚举值
||| @ s 版本字符串("HTTP/1.0", "HTTP/1.1"等)
public export
version : String -> Either HTTPErr Version
version "HTTP/1.0" = Right V10
version "HTTP/1.1" = Right V11
version "HTTP/2.0" = Right V20
version _          = Left InvalidRequest

||| 内容类型枚举
||| 类似 Servant 的 JSON/PlainText 内容类型
public export
data ContentType = JSON | PlainText
