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
  shift
  shift

  ./9ninecc -s "$input" > tmp-$n.s
  (
      echo gcc -o tmp-$n tmp-$n.s "$@"
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
try_stmt_list 3 'foo=1;bar=2;foo+bar;'
try_stmt_list 3 'foo=2;bar=1;return foo+bar;foo-bar;'
try_stmt_list 1 'if(1) return 1; return 2;'
try_stmt_list 2 'if(0) return 1; return 2;'
try_stmt_list 1 'if(1) return 1; else return 2; return 3;'
try_stmt_list 2 'if(0) return 1; else return 2; return 3;'
try_stmt_list 2 'a=100; while (a > 2) a=a-1; return a;'
try_stmt_list 55 'sum=0; for(i=1; i<=10; i=i+1) sum = sum + i; return sum;'
try_stmt_list 1 'n=0; a=0; b=1; for (i=0; i<n; i=i+1) { t=b; b=b+a; a=t; } return b;'
try_stmt_list 1 'n=1; a=0; b=1; for (i=0; i<n; i=i+1) { t=b; b=b+a; a=t; } return b;'
try_stmt_list 2 'n=2; a=0; b=1; for (i=0; i<n; i=i+1) { t=b; b=b+a; a=t; } return b;'
try_stmt_list 3 'n=3; a=0; b=1; for (i=0; i<n; i=i+1) { t=b; b=b+a; a=t; } return b;'
try_stmt_list 5 'n=4; a=0; b=1; for (i=0; i<n; i=i+1) { t=b; b=b+a; a=t; } return b;'
try_stmt_list 100 'return f();' test1.c

echo echo OK >> tmp.sh

$RUN bash tmp.sh

