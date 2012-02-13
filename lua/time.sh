#!/usr/bin/env bash

LUA_BIN_SW=/home/user/Desktop/lua/bin/lua
LUA_BIN_IT=/home/user/Dropbox/Studier/1112q34/VM/src/lua/bin/lua
LUA_TESTS=/home/user/Dropbox/Studier/1112q34/VM/src/lua/test
JMI_TESTS=/home/user/Dropbox/Studier/1112q34/VM/lua

function lua_tests {
  for i in `seq $2`; do
    cd $LUA_TESTS
    $1 array.lua
    $1 bisect.lua
    $1 cf.lua
    $1 dump.lua
    $1 hello.lua
    $1 loop.lua
    $1 save.lua
    $1 sort.lua
    $1 split.lua
    $1 type.lua
  done
}

function jmi_tests {
  for i in `seq $2`; do
    cd $JMI_TESTS
    $1 "arg={6}" ackermann.lua \
    "arg={500000}" ary.lua \
    "arg={500000}" ary2.lua \
    "arg={3000}" ary3.lua \
    "arg={200000}" deep_return.lua \
    "arg={31}" fibo.lua \
    "arg={26000}" hash.lua \
    "arg={55}" hash2.lua \
    "arg={50000}" heapsort.lua \
    "arg={9000}" lists.lua \
    "arg={100}" matrix.lua \
    "arg={10000}" n_body.lua \
    "arg={12}" nestedloop.lua \
    "arg={1000000}" random.lua \
    "arg={500000}" sieve.lua \
    "arg={500}" strcat.lua \
    "arg={600}" strcat2.lua
  done
}

echo "Switch-based - Lua tests"
#time lua_tests $LUA_BIN_SW 1000 > /dev/null

echo
echo "Switch-based - JMI tests"
time jmi_tests $LUA_BIN_SW 4 30 > /dev/null

echo
echo "Indirect Threading - Lua tests"
#time lua_tests $LUA_BIN_IT 1000 > /dev/null

echo
echo "Indirect Threading - JMI tests"
time jmi_tests $LUA_BIN_IT 4 30 > /dev/null

