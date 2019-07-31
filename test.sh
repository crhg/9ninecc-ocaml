#!/bin/bash
RUN=

case $(uname) in
Linux)
    ;;
*)
    RUN="docker-compose run 9ninecc-env"
    ;;
esac

n=1
echo > tmp.sh

try() {
  expected="$1"
  input="$2"

  ./9ninecc -s "$input" > tmp-$n.s
  (
      echo gcc -o tmp-$n tmp-$n.s
      echo ./tmp-$n
      echo 'actual="$?"'

      echo 'if [ "$actual" = "'"$expected"'" ]; then'
      echo '  echo "'"$input"' => $actual"'
      echo else
      echo '  echo "'"$input"' => '"$expected"' expected, but got $actual"'
      echo exit 1
      echo fi
      echo
  ) >> tmp.sh
  n=$(expr $n + 1 )
}

try 0 0
try 42 42
try 41 " 12 + 34 - 5 "
try 19 " 4 * 5 - 3 / 2"
try 21 "(1 + 2) * (3 + 4)"
try 10 "-10 + 20"
try 10 "-10 + +20"
try 1 "1 == 1"
try 0 "1 != 1"
try 1 "2>1"
try 0 "1>1"
try 0 "0>1"
try 1 "2>1"
try 1 "1>=1"
try 0 "0>1"
try 0 "2<1"
try 0 "1<1"
try 1 "0<1"
try 0 "2<=1"
try 1 "1<=1"
try 1 "0<=1"

$RUN bash tmp.sh

echo OK
