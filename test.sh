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

try_expr() {
  expected="$1"
  input="$2;"

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

try_stmt_list() {
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

try_expr 0 0
try_expr 42 42
try_expr 41 " 12 + 34 - 5 "
try_expr 19 " 4 * 5 - 3 / 2"
try_expr 21 "(1 + 2) * (3 + 4)"
try_expr 10 "-10 + 20"
try_expr 10 "-10 + +20"
try_expr 1 "1 == 1"
try_expr 0 "1 != 1"
try_expr 1 "2>1"
try_expr 0 "1>1"
try_expr 0 "0>1"
try_expr 1 "2>1"
try_expr 1 "1>=1"
try_expr 0 "0>1"
try_expr 0 "2<1"
try_expr 0 "1<1"
try_expr 1 "0<1"
try_expr 0 "2<=1"
try_expr 1 "1<=1"
try_expr 1 "0<=1"
try_stmt_list 1 'a=1;a;'
try_stmt_list 3 'a=1;b=2;a+b;'

$RUN bash tmp.sh

echo OK
