module Disassemble (dis) where

import Code (Code(..))
import OP (Op)
import OP qualified
import Data.List (intercalate)
import Text.Printf (printf)

dis :: Code -> String
dis Code{chunk=ops} = loop 0 ops
  where
    loop :: Int -> [Op] -> String
    loop ip = \case
      [] -> ""
      OP.ARG i:ops -> do
        printf "%d: %d\n%s" ip i (loop (ip+1) ops)

      op:ops -> do
        let (args,ops') = getArgs op ops
        let n = length args
        let
          argString =
            case args of
              [] -> ""
              _ -> "(" ++ intercalate "," (map (decode (1+ip+n)) args) ++ ")"

        printf "%d: %s%s\n%s" ip (show op) argString (loop (ip+1+n) ops')


decode :: Int -> (Int,ArgDesc) -> String
decode ip (n,desc) = case desc of
  N -> printf "%d" n
  D -> printf "LAB-%d" (ip + n)

getArgs :: Op -> [Op] -> ([(Int,ArgDesc)], [Op])
getArgs op ops = loop [] ops descs0
  where
    descs0 = argsDescs op
    loop acc ops = \case
      [] -> (reverse acc,ops)
      desc:descs ->
        case ops of
          [] -> error "getArgs/[]"
          OP.ARG n:ops -> loop ((n,desc) : acc) ops descs
          _:_ -> error "getArgs/not-arg"

data ArgDesc = N | D

argsDescs :: Op -> [ArgDesc]
argsDescs = \case
  OP.NUMBER -> [N]
  OP.STRING -> [N]
  OP.GET_LOCAL -> [N]
  OP.GET_UPVALUE -> [N]
  OP.JUMP -> [D]
  OP.JUMP_IF_FALSE -> [D]
  OP.CALL -> [N]
  OP.CLOSURE -> [N,D]
  _op -> [] --error (show _op)
