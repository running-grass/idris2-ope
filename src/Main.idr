||| Main模块是HTTP服务器的入口点
||| 负责配置和启动服务器
module Main

import Ope.Server.Type
import Ope.Server
import App

||| 程序主入口函数
||| 创建服务器配置，并启动HTTP服务器
covering
main : IO ()
main = do
  -- 创建服务器配置，设置最大连接数为1
  let config = { maxConns := 1 } defaultConfig
  putStrLn "Starting server on the http://localhost:\{show config.bind.port}"
  -- 运行服务器，传入配置和应用程序
  runServer $ server config app
