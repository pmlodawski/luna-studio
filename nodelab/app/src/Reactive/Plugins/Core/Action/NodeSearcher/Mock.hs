{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher.Mock where

import           Utils.PreludePlus

import qualified Data.Map as Map
import           Data.Map                   ( Map )

import           Data.Text.Lazy             ( Text )

import           JS.NodeSearcher
import           Reactive.Plugins.Core.Action.NodeSearcher.Scope

mockArray   = Module $ LunaModule $ Map.fromList [("inspect", Function),("to_s", Function),("to_a", Function),("to_h", Function),("to_ary", Function),("frozen?", Function),("eql?", Function),("hash", Function),("at", Function),("fetch", Function),("first", Function),("last", Function),("concat", Function),("push", Function),("pop", Function),("shift", Function),("unshift", Function),("insert", Function),("each", Function),("each_index", Function),("reverse_each", Function),("length", Function),("size", Function),("empty?", Function),("find_index", Function),("index", Function),("rindex", Function),("join", Function),("reverse", Function),("reverse!", Function),("rotate", Function),("rotate!", Function),("sort", Function),("sort!", Function),("sort_by!", Function),("collect", Function),("collect!", Function),("map", Function),("map!", Function),("select", Function),("select!", Function),("keep_if", Function),("values_at", Function),("delete", Function),("delete_at", Function),("delete_if", Function),("reject", Function),("reject!", Function),("zip", Function),("transpose", Function),("replace", Function),("clear", Function),("fill", Function),("include?", Function),("<=>", Function),("slice", Function),("slice!", Function),("assoc", Function),("rassoc", Function),("+", Function),("*", Function),("-", Function),("&", Function),("|", Function),("uniq", Function),("uniq!", Function),("compact", Function),("compact!", Function),("flatten", Function),("flatten!", Function),("count", Function),("shuffle!", Function),("shuffle", Function),("sample", Function),("cycle", Function),("permutation", Function),("combination", Function),("repeated_permutation", Function),("repeated_combination", Function),("product", Function),("take", Function),("take_while", Function),("drop", Function),("drop_while", Function),("bsearch", Function),("pack", Function),("pretty_print", Function),("pretty_print_cycle", Function),("shelljoin", Function)]
mockInteger = Module $ LunaModule $ Map.fromList [("integer?", Function),("odd?", Function),("even?", Function),("upto", Function),("downto", Function),("times", Function),("succ", Function),("next", Function),("pred", Function),("chr", Function),("ord", Function),("to_i", Function),("to_int", Function),("floor", Function),("ceil", Function),("truncate", Function),("round", Function),("gcd", Function),("lcm", Function),("gcdlcm", Function),("numerator", Function),("denominator", Function),("to_r", Function),("rationalize", Function)]
mockIO      = Module $ LunaModule $ Map.fromList [("reopen", Function),("print", Function),("putc", Function),("puts", Function),("printf", Function),("each", Function),("each_line", Function),("each_byte", Function),("each_char", Function),("each_codepoint", Function),("lines", Function),("bytes", Function),("chars", Function),("codeVector2 Ints", Function),("syswrite", Function),("sysread", Function),("fileno", Function),("to_i", Function),("to_io", Function),("fsync", Function),("fdatasync", Function),("sync", Function),("sync=", Function),("lineno", Function),("lineno=", Function),("readlines", Function),("read_nonblock", Function),("write_nonblock", Function),("readpartial", Function),("read", Function),("write", Function),("gets", Function),("readline", Function),("getc", Function),("getbyte", Function),("readchar", Function),("readbyte", Function),("ungetbyte", Function),("ungetc", Function),("<<", Function),("flush", Function),("tell", Function),("seek", Function),("rewind", Function),("pos", Function),("pos=", Function),("eof", Function),("eof?", Function),("close_on_exec?", Function),("close_on_exec=", Function),("close", Function),("closed?", Function),("close_read", Function),("close_write", Function),("isatty", Function),("tty?", Function),("binmode", Function),("binmode?", Function),("sysseek", Function),("advise", Function),("ioctl", Function),("fcntl", Function),("pid", Function),("inspect", Function),("external_encoding", Function),("internal_encoding", Function),("set_encoding", Function),("autoclose?", Function),("autoclose=", Function),("stat", Function),("raw", Function),("raw!", Function),("cooked", Function),("cooked!", Function),("getch", Function),("echo=", Function),("echo?", Function),("noecho", Function),("winsize", Function),("winsize=", Function),("iflush", Function),("oflush", Function),("ioflush", Function)]


mockMD5     = Module $ LunaModule $ Map.fromList [("base64digest", Function),("digest", Function),("file", Function),("hexdigest", Function),("digest_length", Function),("inspect", Function)]
mockSHA1    = Module $ LunaModule $ Map.fromList [("reset", Function),("update", Function),("block_length", Function),("digest_length", Function),("inspect", Function)]
mockDigest  = Module $ LunaModule $ Map.fromList [("hexencode", Function), ("MD5", mockMD5), ("SHA1", mockSHA1)]

mockData    = LunaModule $ Map.fromList [("IO", mockIO), ("Integer", mockInteger), ("Array", mockArray), ("Digest", mockDigest)]

getItemsSearch :: Text -> [QueryResult]
getItemsSearch expr = searchInScope mockData expr

getItemsTree :: Text -> [QueryResult]
getItemsTree prefix = moduleItems mockData prefix

