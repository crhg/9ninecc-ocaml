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

      echo 'if [ "$actual" = "'$expected'" ]; then'
      echo '  echo "'$input' => $actual"'
      echo else
      echo '  echo "'$input' => '$expected' expected, but got $actual"'
      echo exit 1
      echo fi
      echo
  ) >> tmp.sh
  n=$(expr $n + 1 )
}

try 0 0
try 42 42

$RUN bash tmp.sh

echo OK
