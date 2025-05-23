module Ope.WAI.Response

import Ope.WAI.Core

||| 响应数据类型
||| 统一处理不同类型的响应
public export
data Response : Type where
  ||| JSON 响应，直接使用字符串表示 JSON
  JSONResponse : String -> Response
  ||| 纯文本响应，要求值类型实现 Show 接口
  PlainTextResponse : Show a => a -> Response

||| 404 未找到响应
||| 当请求的路径无法匹配到任何路由时返回
public export
notFoundResponse : Response
notFoundResponse = PlainTextResponse "Not Found"

||| 响应渲染函数
||| 将 Response 转换为字符串，用于 HTTP 响应
||| @ resp 待渲染的响应对象
public export
renderResponse : Response -> String
renderResponse (JSONResponse jsonStr) = jsonStr
renderResponse (PlainTextResponse value) = show value

