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

  ./9ninecc -s "main(){$input}" > tmp-$n.s
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

  ./9ninecc -s "main(){$input}" > tmp-$n.s
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

try_decl_list() {
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
try_stmt_list 6 'return add(1, add(2, 3));' test2.c
try_stmt_list 36 'return add8(1,2,3,4,5,6,7,8);' test2.c
try_decl_list 0 'main() { return fib(0); } fib(n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 1 'main() { return fib(1); } fib(n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 1 'main() { return fib(2); } fib(n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 2 'main() { return fib(3); } fib(n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 3 'main() { return fib(4); } fib(n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 5 'main() { return fib(5); } fib(n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 8 'main() { return fib(6); } fib(n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 13 'main() { return fib(7); } fib(n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 3 'main() {return f(1,2,3);} f(x1,x2,x3){return x3;}'
try_decl_list 4 'main() {return f(1,2,3,4);} f(x1,x2,x3,x4){return x4;}'
try_decl_list 5 'main() {return f(1,2,3,4,5);} f(x1,x2,x3,x4,x5){return x5;}'
try_decl_list 6 'main() {return f(1,2,3,4,5,6);} f(x1,x2,x3,x4,x5,x6){return x6;}'
try_decl_list 7 'main() {return f(1,2,3,4,5,6,7);} f(x1,x2,x3,x4,x5,x6,x7){return x7;}'
try_decl_list 8 'main() {return f(1,2,3,4,5,6,7,8);} f(x1,x2,x3,x4,x5,x6,x7,x8){return x8;}'
try_decl_list 1 'main() {x = 1; p = &x; y = *p; return y; }'

echo echo OK >> tmp.sh

$RUN bash tmp.sh

