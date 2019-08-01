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

rm tmp-*.s

try_expr() {
  expected="$1"
  input="$2;"

  if ./9ninecc -s "int main(){$input}" > tmp-$n.s; then
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
  else
      echo "compile failed: $input"
      exit 1;
  fi
  n=$(expr $n + 1 )
}

try_stmt_list() {
    expected="$1"
    input="$2"
    shift
    shift

    if ./9ninecc -s "int main(){$input}" > tmp-$n.s; then
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
    else
        echo "compile failed: $input"
        exit 1;
    fi
    n=$(expr $n + 1 )
}

try_decl_list() {
    expected="$1"
    input="$2"
    shift
    shift

    if ./9ninecc -s "$input" > tmp-$n.s; then
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
    else
        echo "compile failed: $input"
        exit 1;
    fi
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
try_stmt_list 1 'int a; a=1;a;'
try_stmt_list 3 'int a; int b; a=1;b=2;a+b;'
try_stmt_list 3 'int foo; int bar; foo=1;bar=2;foo+bar;'
try_stmt_list 3 'int foo; int bar; foo=2;bar=1;return foo+bar;foo-bar;'
try_stmt_list 1 'if(1) return 1; return 2;'
try_stmt_list 2 'if(0) return 1; return 2;'
try_stmt_list 1 'if(1) return 1; else return 2; return 3;'
try_stmt_list 2 'if(0) return 1; else return 2; return 3;'
try_stmt_list 2 'int a; a=100; while (a > 2) a=a-1; return a;'
try_stmt_list 55 'int sum; int i; sum=0; for(i=1; i<=10; i=i+1) sum = sum + i; return sum;'
try_stmt_list 1 'int n; int a; int b; int i; n=0; a=0; b=1; for (i=0; i<n; i=i+1) { int t; t=b; b=b+a; a=t; } return b;'
try_stmt_list 1 'int n; int a; int b; int i; n=1; a=0; b=1; for (i=0; i<n; i=i+1) { int t; t=b; b=b+a; a=t; } return b;'
try_stmt_list 2 'int n; int a; int b; int i; n=2; a=0; b=1; for (i=0; i<n; i=i+1) { int t; t=b; b=b+a; a=t; } return b;'
try_stmt_list 3 'int n; int a; int b; int i; n=3; a=0; b=1; for (i=0; i<n; i=i+1) { int t; t=b; b=b+a; a=t; } return b;'
try_stmt_list 5 'int n; int a; int b; int i; n=4; a=0; b=1; for (i=0; i<n; i=i+1) { int t; t=b; b=b+a; a=t; } return b;'
try_stmt_list 100 'return f();' test1.c
try_stmt_list 6 'return add(1, add(2, 3));' test2.c
try_stmt_list 36 'return add8(1,2,3,4,5,6,7,8);' test2.c
try_decl_list 0 'int main() { return fib(0); } int fib(int n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 1 'int main() { return fib(1); } int fib(int n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 1 'int main() { return fib(2); } int fib(int n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 2 'int main() { return fib(3); } int fib(int n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 3 'int main() { return fib(4); } int fib(int n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 5 'int main() { return fib(5); } int fib(int n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 8 'int main() { return fib(6); } int fib(int n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 13 'int main() { return fib(7); } int fib(int n) { if (n==0) return 0; if (n==1) return 1; return fib(n-1)+fib(n-2);}'
try_decl_list 3 'int main() {return f(1,2,3);} int f(int x1,int x2,int x3){return x3;}'
try_decl_list 4 'int main() {return f(1,2,3,4);} int f(int x1,int x2,int x3,int x4){return x4;}'
try_decl_list 5 'int main() {return f(1,2,3,4,5);} int f(int x1,int x2,int x3,int x4,int x5){return x5;}'
try_decl_list 6 'int main() {return f(1,2,3,4,5,6);} int f(int x1,int x2,int x3,int x4,int x5,int x6){return x6;}'
try_decl_list 7 'int main() {return f(1,2,3,4,5,6,7);} int f(int x1,int x2,int x3,int x4,int x5,int x6,int x7){return x7;}'
try_decl_list 8 'int main() {return f(1,2,3,4,5,6,7,8);} int f(int x1,int x2,int x3,int x4,int x5,int x6,int x7,int x8){return x8;}'
try_decl_list 1 'int main() {int x; int *p; int y; x = 1; p = &x; y = *p; return y; }'
try_decl_list 1 'int main() {int x; int y; x = 1; y = 2; return *(&y + 1);}'
try_decl_list 2 'int main() {int x; int y; int z; return &x - &z; }'
try_decl_list 2 'int main() {int x; int y; x = 1; y = 2; return *(&x - 1);}'
try_decl_list 4 'int main() {int x; return sizeof x; }'
try_decl_list 8 'int main() {int x; return sizeof &x; }'
try_decl_list 32 'int main() {int x[8]; return sizeof x; }'

echo echo OK >> tmp.sh

$RUN bash tmp.sh

