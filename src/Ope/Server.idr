||| Server模块提供HTTP服务器的核心功能
||| 实现了一个基于异步IO的HTTP服务器框架
module Ope.Server

import public Data.SortedMap
import Data.ByteVect as BV

import public FS.Posix
import public FS.Socket

import public IO.Async.Loop.Posix
import public IO.Async.Loop.Epoll

import public System

import Derive.Prelude

import Ope.Server.Type

import Ope.WAI.Core
import Ope.WAI.Request
import Ope.WAI.Response

%default total

||| Prog类型是服务器异步流程序的核心类型
||| 基于Poll事件循环和AsyncStream实现异步IO
public export
0 Prog : List Type -> Type -> Type
Prog = AsyncStream Poll

||| 运行HTTP服务器
||| 
||| 接收一个异步程序并在epoll事件循环中执行它
||| @ prog 要执行的服务器程序
export covering
runServer : Prog [Errno] Void -> IO ()
runServer prog = epollApp $ mpull (handle [stderrLn . interpolate] prog)


%inline
MaxHeaderSize, MaxContentSize : Nat
MaxHeaderSize = 0xffff
MaxContentSize = 0xffff_ffff

%inline
SPACE, COLON : Bits8
SPACE = 32
COLON = 58

||| 解析HTTP请求的起始行
||| 
||| 提取HTTP方法、目标路径和HTTP版本
||| @ bs 包含起始行的字节串
startLine : ByteString -> Either HTTPErr (Method,String,Version)
startLine bs =
  case toString <$> split SPACE (trim bs) of
    [m,t,v] => [| (\x,y,z => (x,y,z)) (method m) (pure t) (version v) |]
    _       => Left InvalidRequest


||| 解析HTTP请求的头部字段
||| 
||| 递归处理头部列表，构建头部映射
||| @ hs 当前已解析的头部映射
||| @ t 待解析的头部字节串列表
headers : Headers -> List ByteString -> Either HTTPErr Headers
headers hs []     = Right hs
headers hs (h::t) =
  case break (COLON ==) h of
    (xs,BS (S k) bv) =>
     let name := toLower (toString xs)
         val  := toString (trim $ tail bv)
      in headers (insert name val hs) t
    _                => Left InvalidRequest


||| 从HTTP头部获取内容长度
||| @ hs HTTP头部映射
contentLength : Headers -> Nat
contentLength = maybe 0 cast . lookup "content-length"

||| 从HTTP头部获取内容类型
||| @ hs HTTP头部映射
contentType : Headers -> Maybe String
contentType = lookup "content-type"


||| 组装HTTP请求对象
||| 
||| 将解析好的HTTP组件组装成完整的Request对象
||| @ p 包含HTTP字节流的异步拉取流
export
assemble :
     HTTPPull (List ByteString) (HTTPStream ByteString)
  -> HTTPPull o (Maybe Request)
assemble p = Prelude.do
  Right (h,rem) <- C.uncons p | _ => pure Nothing
  (met,uri,vrs) <- injectEither (startLine h)
  (hs,body)     <- foldPairE headers empty rem
  let cl := contentLength hs
      ct := contentType hs
      queryParams := SortedMap.fromList $ []
  when (cl > MaxContentSize) (throw ContentSizeExceeded)
  pure $ Just (MkRequest met uri queryParams vrs hs cl ct $ C.take cl body)


||| 从HTTP字节流解析HTTP请求
||| 
||| @ req HTTP字节流
export
request : HTTPStream ByteString -> HTTPPull o (Maybe Request)
request req =
     breakAtSubstring pure "\r\n\r\n" req
  |> C.limit HeaderSizeExceeded MaxHeaderSize
  |> lines
  |> assemble


||| 编码HTTP响应
||| 
||| 生成包含状态码和响应体的HTTP响应字节串
||| @ status HTTP状态码
||| @ body 响应体内容
export
encodeResponse' : (status : Nat) -> String -> ByteString
encodeResponse' status body =
  fromString $
    statusStr ++
    contentLengthStr ++
    "\r\n" ++
    body
  where
    statusStr : String
    statusStr = "HTTP/1.1 \{show status}\r\n"
    contentLengthStr : String
    contentLengthStr = "Content-Length: \{show (length body)}\r\n"

||| 生成400 Bad Request响应
export
badRequest : ByteString
badRequest = encodeResponse' 400 ""

||| 应用程序包装器
||| 
||| 将应用程序的响应转换为HTTP字节流
||| @ app 应用程序处理函数
||| @ req HTTP请求对象
export
applicationWraper : Application -> Request -> HTTPStream ByteString
applicationWraper app req = do
  liftIO (app req) >>= \res =>
    emit (encodeResponse' 200 (renderResponse res))


||| 处理HTTP请求
||| 
||| 从客户端读取请求，调用应用处理函数，并将响应写回客户端
||| @ cli 客户端socket
||| @ app 应用程序处理函数
||| @ p HTTP请求解析器
handleRequest :
     Socket AF_INET
  -> Application
  -> HTTPPull ByteString (Maybe Request)
  -> AsyncStream Poll [Errno] Void
handleRequest cli app p =
  extractErr HTTPErr (writeTo cli (p >>= response)) >>= \case
    Left _   => emit badRequest |> writeTo cli
    Right () => pure ()
  where
  response : Maybe Request -> HTTPStream ByteString
  response Nothing  = pure ()
  response (Just r) = applicationWraper app r

||| 处理单个客户端连接
||| 
||| 读取客户端请求并返回响应，完成后关闭连接
||| @ app 应用程序处理函数
||| @ cli 客户端socket
covering
serve : Application -> Socket AF_INET -> Async Poll [] ()
serve app cli =
  flip guarantee (close' cli) $
    mpull $ handleErrors (\(Here x) => stderrLn "\{x}") $
         bytes cli 0xfff
      |> request
      |> handleRequest cli app


||| 服务器配置记录类型
||| 
||| 包含服务器绑定地址和最大连接数
public export
record ServerConfig where
  constructor MkServerConfig
  ||| 服务器绑定的IP地址和端口
  bind : IP4Addr
  ||| 服务器允许的最大连接数
  maxConns : Nat

||| 默认服务器配置
||| 
||| 绑定127.0.0.1:2222，最大连接数128
public export
defaultConfig : ServerConfig
defaultConfig = MkServerConfig  {
    bind = IP4 [127,0,0,1] 2222,
    maxConns = 128
  }

||| 创建并启动HTTP服务器
||| 
||| 根据提供的配置和应用程序创建HTTP服务器
||| @ config 服务器配置
||| @ app 应用程序处理函数
covering
public export
server : ServerConfig -> Application -> Prog [Errno] Void
server config app =
  let conn = acceptOn AF_INET SOCK_STREAM config.bind
      serve' = serve app
  in
  case config.maxConns of
    S k => foreachPar (S k) serve' conn
    Z   => foreachPar 1 serve' conn
