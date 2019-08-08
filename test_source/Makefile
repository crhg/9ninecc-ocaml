SRCS=$(sort tmp.c test_main.c $(filter-out test.c dummy_main.c,$(wildcard *.c)))
OBJS=$(SRCS:.c=.o)
CC=gcc
CFLAGS=-g
LDFLAGS=-no-pie

tmp: $(OBJS)
	$(CC) -o tmp $(OBJS) $(LDFLAGS)

$(OBJS): try.h

test_main.c: gen_test_main.pl tmp.c
	perl gen_test_main.pl tmp.c |clang-format > test_main.c

tmp.c: gen_tmp.pl test.c
	perl gen_tmp.pl test.c $(ONLY) > tmp.c

tmp.s: ../9ninecc tmp.c
	env OCAMLRUNPARAM=b ../9ninecc tmp.c > tmp.s || (mv tmp.s failed-tmp.s && exit 1)

tmp.o: tmp.s
	$(CC) -c tmp.s

.PHONY: test
test:
	make clean
	make test-main

.PHONY: test-main
test-main: tmp
	./tmp

.PHONY: clean
clean:
	rm -f *~ tmp* *.o test_main.c test.s
