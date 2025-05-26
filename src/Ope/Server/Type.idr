||| Server.Type模块定义了HTTP服务器的核心类型
||| 包括请求、响应和处理函数的类型定义
module Ope.Server.Type

import FS.Posix
import FS.Socket

import IO.Async.Loop.Posix
import IO.Async.Loop.Epoll

import Data.SortedMap
import Derive.Prelude
import System

import public Ope.WAI.Core
import Ope.WAI.Request
import Ope.WAI.Response
