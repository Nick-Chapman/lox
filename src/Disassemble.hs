module Disassemble (dis) where

import Code (Code(..))
import OP (Op)
import OP qualified
import Pos (Pos)
import Data.List (intercalate)
import Text.Printf (printf)

dis :: Code -> String
dis Code{chunk=ops} = loop 0 ops
  where
    loop :: Int -> [(Pos,Op)] -> String
    loop ip = \case
      [] -> ""
      (_,OP.ARG i):pops -> do
        printf "%d: %d\n%s" ip i (loop (ip+1) pops)

      (_,op):pops -> do
        let (args,pops') = getArgs op pops
        let n = length args
        let
          argString =
            case args of
              [] -> ""
              _ -> "(" ++ intercalate "," (map (decode (1+ip+n)) args) ++ ")"

        printf "%d: %s%s\n%s" ip (show op) argString (loop (ip+1+n) pops')


decode :: Int -> (Int,ArgDesc) -> String
decode ip (n,desc) = case desc of
  N -> printf "%d" n
  D -> printf "LAB-%d" (ip + n)

getArgs :: Op -> [(Pos,Op)] -> ([(Int,ArgDesc)], [(Pos,Op)])
getArgs op pops = loop [] pops descs0
  where
    descs0 = argsDescs op
    loop acc pops = \case
      [] -> (reverse acc,pops)
      desc:descs ->
        case pops of
          [] -> error "getArgs/[]"
          (_pos,OP.ARG n):pops -> loop ((n,desc) : acc) pops descs
          _:_ -> error "getArgs/not-arg"

data ArgDesc = N | D

argsDescs :: Op -> [ArgDesc]
argsDescs = \case
  OP.NUMBER -> [N]
  OP.STRING -> [N]
  OP.GET_LOCAL -> [N]
  OP.GET_UPVALUE -> [N]
  OP.SET_UPVALUE -> [N]
  OP.JUMP -> [D]
  OP.JUMP_IF_FALSE -> [D]
  OP.CALL -> [N]
  OP.CLOSURE -> [N,D]
  _op -> [] --error (show _op)
