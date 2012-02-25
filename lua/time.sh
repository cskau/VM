#!/usr/bin/env bash

LUA_BIN_SW=/home/user/Desktop/lua/bin/lua
LUA_BIN_IT=/home/user/Dropbox/Studier/1112q34/VM/src/lua/bin/lua
LUA_TESTS=/home/user/Dropbox/Studier/1112q34/VM/src/lua/test
JMI_TESTS=/home/user/Dropbox/Studier/1112q34/VM/lua

BTIME=/usr/bin/time -f "%U %S %E %P"

function repeat_tests {
  echo
  echo "$4 $3 "
  for i in `seq $2`; do
    $1 $3 $4 > /dev/null
  done
}

function lua_tests {
  cd $LUA_TESTS
  time repeat_tests $1 $2 array.lua
  time repeat_tests $1 $2 bisect.lua
  time repeat_tests $1 $2 cf.lua
  time repeat_tests $1 $2 dump.lua
  time repeat_tests $1 $2 hello.lua
  time repeat_tests $1 $2 loop.lua
  time repeat_tests $1 $2 save.lua
  time repeat_tests $1 $2 sort.lua
  time repeat_tests $1 $2 split.lua
  time repeat_tests $1 $2 type.lua
}

function jmi_tests {
  cd $JMI_TESTS
  time repeat_tests $1 $2 "arg={6}" ackermann.lua
  time repeat_tests $1 $2 "arg={500000}" ary.lua
  time repeat_tests $1 $2 "arg={500000}" ary2.lua
  time repeat_tests $1 $2 "arg={3000}" ary3.lua
  time repeat_tests $1 $2 "arg={200000}" deep_return.lua
  time repeat_tests $1 $2 "arg={31}" fibo.lua
  time repeat_tests $1 $2 "arg={26000}" hash.lua
  time repeat_tests $1 $2 "arg={55}" hash2.lua
  time repeat_tests $1 $2 "arg={50000}" heapsort.lua
  time repeat_tests $1 $2 "arg={9000}" lists.lua
  time repeat_tests $1 $2 "arg={100}" matrix.lua
  time repeat_tests $1 $2 "arg={10000}" n_body.lua
  time repeat_tests $1 $2 "arg={12}" nestedloop.lua
  time repeat_tests $1 $2 "arg={1000000}" random.lua
  time repeat_tests $1 $2 "arg={500000}" sieve.lua
  time repeat_tests $1 $2 "arg={500}" strcat.lua
  time repeat_tests $1 $2 "arg={600}" strcat2.lua
}

echo "Switch-based - Lua tests"
#time lua_tests $LUA_BIN_SW 1000 > /dev/null

echo
echo "Switch-based - JMI tests"
jmi_tests $LUA_BIN_SW 4

echo
echo "Indirect Threading - Lua tests"
#time lua_tests $LUA_BIN_IT 1000 > /dev/null

echo
echo "Indirect Threading - JMI tests"
jmi_tests $LUA_BIN_IT 4

