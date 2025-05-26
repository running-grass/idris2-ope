||| The Server.Type module defines the core types of the HTTP server
||| Including definitions for request, response, and handler function types
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
