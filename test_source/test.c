extern int pr_int();
extern int try_printf();
extern int vsprintf();
extern int printf();

// @try_ret test0 0
int test0() { 0; }
// @end

// @try_ret test1 42
int test1() { 42; }
// @end

// @try_ret test2 21
int test2() { 5 + 20 - 4; }
// @end

// @try_ret test3 41
int test3() { 12 + 34 - 5; }
// @end

// @try_ret test4 47
int test4() { 5 + 6 * 7; }
// @end

// @try_ret test5 6
int test5() { 1 + 10 / 2; }
// @end

// @try_ret test6 15
int test6() { 5 * (9 - 6); }
// @end

// @try_ret test7 4
int test7() { (3 + 5) / 2; }
// @end

// @try_ret test8 5
int test8() { -10 + 15; }
// @end

// @try_ret test9 10
int test9() { -2 * -(2 + 3); }
// @end

// @try_ret test10 1
int test10() { 1 == 1; }
// @end

// @try_ret test11 0
int test11() { 1 == 0; }
// @end

// @try_ret test12 0
int test12() { 3 * 2 != 6; }
// @end

// @try_ret test13 1
int test13() { 1 + 10 / 2 != 5; }
// @end

// @try_ret test14 1
int test14() { 2 > 1; }
// @end

// @try_ret test15 0
int test15() { 2 > 2; }
// @end

// @try_ret test16 0
int test16() { 2 > 3; }
// @end

// @try_ret test17 1
int test17() { 2 >= 1; }
// @end

// @try_ret test18 1
int test18() { 2 >= 2; }
// @end

// @try_ret test19 0
int test19() { 2 >= 3; }
// @end

// @try_ret test20 0
int test20() { 2 < 1; }
// @end

// @try_ret test21 0
int test21() { 2 < 2; }
// @end

// @try_ret test22 1
int test22() { 2 < 3; }
// @end

// @try_ret test23 0
int test23() { 2 <= 1; }
// @end

// @try_ret test24 1
int test24() { 2 <= 2; }
// @end

// @try_ret test25 1
int test25() { 2 <= 3; }
// @end

// @try_ret test26 4
int test26() {
  int a;
  a = 2;
  a * 2;
}
// @end

// @try_ret test27 3
int test27() {
  int a;
  int z;
  a = 1;
  z = 2;
  a + z;
}
// @end

// @try_ret test28 2
int test28() {
  return 2;
  3;
}
// @end

// @try_ret test29 3
int test29() {
  int abc;
  int abd;
  abc = 1;
  abd = 2;
  abc + abd;
}
// @end

// @try_ret test30 2
int test30() {
  if (1)
    return 2;
  return 3;
}
// @end

// @try_ret test31 3
int test31() {
  if (0)
    return 2;
  return 3;
}
// @end

// @try_ret test32 2
int test32() {
  if (1)
    return 2;
  else
    return 3;
  return 4;
}
// @end

// @try_ret test33 3
int test33() {
  if (0)
    return 2;
  else
    return 3;
  return 4;
}
// @end

// @try_ret test34 5
int test34() {
  int a;
  a = 0;
  while (a != 5)
    a = a + 1;
  return a;
}
// @end

// @try_ret test35 0
int test35() {
  int a;
  a = 0;
  while (a != 9999999)
    a = a + 1;
  return 0;
}
// @end

// @try_ret test36 50
int test36() {
  int a;
  int b;
  b = 0;
  for (a = 0; a < 5; a = a + 1)
    b = b + 10;
  return b;
}
// @end

// @try_ret test37 0
int test37() {
  int a;
  for (a = 0; a != 9999999; a = a + 1)
    return 0;
}
// @end

// @try_ret test38 3
int test38() {
  {
    int a;
    a = 1;
    a = a + 2;
    return a;
  }
}
// @end

// @try_out test39 OK
extern int foo1();
int test39() { foo1(); }
// @end

// @try_ret test40 45
extern int foo2();
int test40() { return 1 + foo2() + 2; }
// @end

// @try_out test41 1-2-3-4-5-6-7-8
extern int foo3();
int test41() { foo3(1, 2, 3, 4, 5, 6, 7, 8); }
// @end

// @try_out test42 "5"
int add(int a, int b) { return a + b; }
int test42() { pr_int(add(2, 3)); }
// @end

/* レジスタ名と同じ関数名が使えるかどうかのテスト */
/* intel-syntaxだと予約語で使えないので今のところ通らない */
/* // @try_out test42_2 "5" */
/* int r10(int a, int b) { return a + b; } */
/* int test42_2() { pr_int(r10(2, 3)); } */
/* // @end */

// @try_out test43 "6765"
int fib(int n) {
  if (n <= 2)
    return 1;
  else
    return fib(n - 1) + fib(n - 2);
}
int test43() { pr_int(fib(20)); }
// @end

// @try_out test44 3
int test44() {
  int x;
  x = 3;
  int *y;
  pr_int(3);
}
// @end

// @try_out test45 3
int test45() {
  int x;
  x = 3;
  int *y;
  y = &x;
  int z;
  z = *y;
  pr_int(z);
}
// @end

// @try_out test46 3
int test46() {
  int x;
  x = 3;
  int *y;
  y = &x;
  pr_int(*y);
}
// @end

int getdata();
// @try_out test47 10
int test47() {
  int *p;
  p = getdata();
  pr_int(*p);
}
// @end

// @try_out test48 30
int test48() {
  int *p;
  p = getdata();
  pr_int(*(p + 2));
}
// @end

int getdata2();
// @try_out test49 40
int test49() {
  int **p;
  p = getdata2();
  pr_int(**p);
}
// @end

// @try_out test50 20
int test50() {
  int **p;
  p = getdata2();
  pr_int(**(p + 2));
}
// @end

// @try_out test51 30
int test51() {
  int *p;
  p = getdata();
  p = p + 2;
  pr_int(*p);
}
// @end

// @try_out test52 20
int test52() {
  int *p;
  p = getdata();
  p = p + 2;
  pr_int(*(p - 1));
}
// @end

// @try_out test53 20
int test53() {
  int **p;
  p = getdata2();
  p = p + 2;
  pr_int(**p);
}
// @end

// @try_out test54 30
int test54() {
  int **p;
  p = getdata2();
  p = p + 2;
  pr_int(**(p - 1));
}
// @end

extern int at();
// @try_out test55 2
int test55() {
  int *p;
  int *q;
  p = at(1);
  q = at(3);
  pr_int(q - p);
}
// @end

// @try_out test56 -2
int test56() {
  int *p;
  int *q;
  p = at(1);
  q = at(3);
  pr_int(p - q);
}
// @end

// @try_ret test57 4
int test57() { return sizeof(5); }
// @end

// @try_ret test58 4
int test58() {
  int a;
  return sizeof(a);
}
// @end

// @try_ret test59 8
int test59() {
  int a;
  return sizeof(&a);
}
// @end

// @try_ret test60 8
int test60() {
  int *a;
  return sizeof(a);
}
// @end

// @try_ret test61 4
int test61() {
  int *a;
  return sizeof(*a);
}
// @end

// @try_ret test62 0
int test62() {
  int a[10];
  return 0;
}
// @end

// @try_out test63 10
int test63() {
  int *p;
  p = getdata();
  pr_int(p[0]);
}
// @end

// @try_out test64 30
int test64() {
  int *p;
  p = getdata();
  pr_int(p[2]);
}
// @end

// @try_out test65 10
int test65() {
  int *p;
  p = getdata();
  pr_int(0 [p]);
}
// @end

// @try_out test66 30
int test66() {
  int *p;
  p = getdata();
  pr_int(2 [p]);
}
// @end

// @try_out test67 40
int test67() {
  int **p;
  p = getdata2();
  pr_int(*p[0]);
}
// @end

// @try_out test68 20
int test68() {
  int **p;
  p = getdata2();
  pr_int(*p[2]);
}
// @end

// @try_out test69 40
int test69() {
  int **p;
  p = getdata2();
  pr_int(*0 [p]);
}
// @end

// @try_out test70 20
int test70() {
  int **p;
  p = getdata2();
  pr_int(*2 [p]);
}
// @end

// @try_ret test71 1
int test71() {
  int a;
  int *p;
  p = &a;
  *p = 1;
  return a;
}
// @end

// @try_out test72 1
int test72() {
  int a[10];
  a[0] = 1;
  pr_int(a[0]);
}
// @end

// @try_out test73 1
int test73() {
  int a[10];
  int *p;
  p = a;
  *p = 1;
  pr_int(a[0]);
}
// @end

// @try_out test74 1
int test74() {
  int a[10];
  int *p;
  a[5] = 1;
  p = &a[5];
  pr_int(*p);
}
// @end

// @try_out test75 5
int test75() {
  int a[10];
  pr_int(&a[5] - a);
}
// @end

// @try_out test76 10
int a76;
int f76(int x) { a76 = x; }
int test76() {
  f76(10);
  pr_int(a76);
}
// @end

// @try_out test77 100
int g77() { return 100; }
int a77;
int f77(int x) { a77 = x; }
int test77() {
  f77(g77());
  pr_int(a77);
}
// @end

// @try_out test78 10
int a78[10];
int f78(int x) { a78[3] = x; }
int test78() {
  f78(10);
  pr_int(a78[3]);
}
// @end

// @try_ret test79 3
int test79() {
  char x[3];
  x[0] = -1;
  /* x[1] = 2; */
  int y;
  y = 4;
  /* y = x[0] + y; */
  y = y + x[0];
  /* return x[0] + y; */
  return y;
}
// @end

// @try_out test80 hoge
int test80() { try_printf("hoge"); }
// @end

// @try_out test81 hoge
int test81() { /* comment */ try_printf("hoge"); }
// @end

// @try_ret test82 80
int a82[10][20];
int test82() {
    return sizeof(a82[1]);
}
// @end

// @try_ret test83 42
int x83 = 42;
int test83() {
    return x83;
}
// @end

// @try_out test84 foo
char *s84 = "foo";
int test84() {
    try_printf("%s", s84);
}
// @end

// @try_out test84_2 !
char s84_2[10];
char *p84_2 = s84_2;
int test84_2() {
    s84_2[0] = 33;
    s84_2[1] = 0;
    try_printf("%s", p84_2);
}
// @end

// @try_out test85 oo
char *s85 = "foo" + 1;
int test85() {
    try_printf("%s", s85);
}
// @end

// @try_ret test86 86
int x86[3];
int *p86 = x86;
int test86() {
    x86[1] = 86;
    return *(p86 + 1);
}
// @end

// @try_ret test87 87
int x87[3];
int *p87 = &x87[1];
int test87() {
    x87[1] = 87;
    return *p87;
}
// @end

// @try_ret test88_0 2
int a88_0[2] = {1,2};
int test88_0() {
    return a88_0[1];
}
// @end

// @try_ret test88 2
int a88[] = {1,2};
int test88() {
    return a88[1];
}
// @end

// @try_ret test89 89
int test89() {
    int x = 89;
    return x;
}
// @end

// @try_out test90 test90
int test90() {
    char *s = "test90";
    try_printf("%s", s);
}
// @end

// @try_out test91 test91
int test91() {
    char s[7] = "test91";
    try_printf("%s", s);
}
// @end

// @try_ret test92 92
int test92() {
    int x[3] = {91, 92};
    return x[1];
}
// @end

// @try_ret test93 1
int test93() {
    int x[] = {91, 92, 93};
    return sizeof(x) == sizeof(x[0]) *3;
}
// @end

// @try_ret test94 1
int test94() {
    char s[] = "hoge";
    return sizeof(s) == 5;
}
// @end

// @try_ret test95 0
int test95() {
    int x = 5;
    while ((x = x - 1) > 0);
    return x;
}
// @end

// @try_ret test96 8
int test96() {
    struct {
        int x;
        int y;
    } a;
    return sizeof(a);
}
// @end

// @try_ret test97 97
int test97() {
    struct {
        int x;
        int y;
    } a;
    (&a)->x = 90;
    (&a)->y = 7;
    return (&a)->x + (&a)->y;
}
// @end

// @try_ret test98 98
int test98() {
    struct {
        int x;
        int y;
    } a;
    a.x = 90;
    a.y = 8;
    return a.x + a.y;
}
// @end

// @try_ret test99 99
int test99() {
    struct {
        int x;
        int y;
    } a[10];
    int i;
    for (i = 0; i < sizeof(a) / sizeof(a[0]); i = i + 1) {
        a[i].x = i * 10;
        a[i].y = i;
    }
    return a[9].x + a[9].y;
}
// @end

// @try_out test100 "2,1,0,"
int test100() {
    struct a {
        struct a *next;
        int x;
    } a[10];

    a[0].next = 0;
    a[0].x = 0;
    a[1].next = &a[0];
    a[1].x = 1;
    a[2].next = &a[1];
    a[2].x = 2;

    struct a *p;
    for (p = &a[2]; p != 0; p = p->next) {
        try_printf("%d,", p->x);
    }
}
// @end

// @try_ret test101 101
struct A101 {
    int x;
};
int test101() {
    struct A101 a101;
    a101.x = 101;

    return a101.x;
}
// @end

// @try_ret test102 102
int test102() {
    struct A {
        int x;
    };

    struct A a;
    a.x = 102;

    return a.x;
}
// @end

/* // @try_ret test103 103 */
/* int test103() { */
/*     union U103 { */
/*         int x; */
/*         char y[10]; */
/*     }; */
/*  */
/*     union U103 u; */
/*  */
/*     u.y[0] = 33; */
/*  */
/*     return u.y[0]; */
/* } */
/* // @end */
/*  */
/* // @try_out test104 ! */
/* int test104() { */
/*     union U104 { */
/*         int x; */
/*         char y[10]; */
/*     }; */
/*  */
/*     union U104 u; */
/*  */
/*     u.y[0] = 33; */
/*     u.y[1] = 0; */
/*  */
/*     try_printf("%s", u.y); */
/* } */
/* // @end */
/*  */
/* // @try_out test105 !A */
/* int test105() { */
/*     union U105 { */
/*         int x; */
/*         char y[10]; */
/*     }; */
/*  */
/*     union U105 u; */
/*  */
/*     u.x = 33 + 65 * 256; // 33 = !, 65 = A */
/*  */
/*     try_printf("%s", u.y); */
/* } */
// @end

/* // @try_out test106 106hoge */
/* int test106() { */
/*     struct S106 { */
/*         int x; */
/*         char y[20]; */
/*     }; */
/*  */
/*     struct S106 s = { .x = 106, .y = "hoge" }; */
/*  */
/*     try_printf("%d%s", s.x, s.y); */
/* } */
/* // @end */
/*  */
/* // @try_ret test107 107 */
/* int test107() { */
/*     union U107 { */
/*         int x; */
/*         char y[5]; */
/*     }; */
/*  */
/*     union U107 u = { .x = 107 }; */
/*  */
/*     return u.x; */
/* } */
/* // @end */
/*  */
/* // @try_out test108 108hoge */
/* struct S108 { */
/*     int x; */
/*     char y[20]; */
/* }; */
/*  */
/* struct S108 s108 = { .x = 108, .y = "hoge" }; */
/*  */
/* int test108() { */
/*     try_printf("%d%s", s108.x, s108.y); */
/* } */
/* // @end */
/*  */
/* // @try_ret test109 109 */
/* union U109 { */
/*     int x; */
/*     char y[5]; */
/* }; */
/*  */
/* union U109 u109 = { .x = 109 }; */
/*  */
/* int test109() { */
/*     return u109.x; */
/* } */
/* // @end */

// @try_ret test111 0
int test111() {
  ({ int x[2][3]; int *y=x; *y=0; **x; });
}
// @end

// @try_ret test112 1
int test112() {
   int x[2][3]; int *y=x; *(y+1)=1; *(*x+1);
}
// @end

// @try_ret test113 4
int test113() {
    sizeof("abc");
}
// @end

// @try_ret test114 11
int test114() {
    "\v"[0];
}
// @end

// @try_ret test115 2
int test115() {
    int x=2; { int x=3; } x;
}
// @end

// @try_ret test116 2
int test116() {
    struct t {char a[2];}; { struct t {char a[4];}; } struct t y; sizeof(y);
}
// @end

// @try_ret test117 4
typedef int INT117;
int test117() {
    INT117 x;
    sizeof(x);
}
// @end

// @try_ret test118 4
int test118() {
    typedef int INT118;;
    INT118 x;
    sizeof(x);
}
// @end

// @try_ret test119 4
int test119() {
    typedef int INT;
    INT x;
    sizeof(x);
}
// @end

// @try_ret test120 4
int test120() {
    typedef int a;
    struct a { int x; } x;
    sizeof(x);
}
// @end

// @try_ret test121 1
int test121() {
    typedef int a;
    struct { char a; } x;
    sizeof(x);
}
// @end

// @try_ret test122 4
int test122() {
    typedef int INT;
    typedef INT INT2;
    INT2 x;
    sizeof(x);
}
// @end

// @try_ret test123 6
int test123() {
    typedef int INT;
    int x = 1;
    {
        INT INT;
        INT = 3;
        x = x + INT;
    }
    INT y = x + 2;
    return y;
}
// @end


// @try_ret test124 80
int test124() {
    typedef int A[20];
    A a;

    return sizeof a;
}
// @end

// @try_ret test125 2
int test125() {
    enum { A, B, C } a;

    a = C;

    return a;
}
// @end

// @try_ret test126 1
int test126() {
    enum e { A, B, C };
    enum e a;

    a = B;

    return a;
}
// @end

// @try_ret test127 12
int test127() {
    enum e { A=10, B, C };
    enum e a;

    a = C;

    return a;
}
// @end

// @try_ret test128 2
int test128() {
    enum { A, B, C, } a;

    a = C;

    return a;
}
// @end

// @try_ret test129 10
#define TEST129 10
int test129() {
    return TEST129;
}
// @end

// @try_ret test130 10
#define test130 test130
int test130() {
    return 10;
}
// @end

// @try_ret test131 10
#define TEST131(x, y) ((x) + (y))
int test131() {
    return TEST131(2, 8);
}
// @end

// @try_ret test132 32
#include "test132.h"
int test132() {
    return TEST132;
}
// @end

// @try_ret test133 1
#define TEST133 0
#if 1
#define TEST133 1
#endif
int test133() {
    return TEST133;
}
// @end

// @try_ret test134 0
#if 0
#define TEST134 1
#else
#define TEST134 0
#endif
int test134() {
    return TEST134;
}
// @end

// @try_ret test135 1
#define TEST135_1 10
#if TEST135_1 > 5
#define TEST135 1
#else
#define TEST135 0
#endif
int test135() {
    return TEST135;
}
// @end

// @try_ret test136 1
#define TEST136_1 10
#if defined TEST136_1
#define TEST136 1
#else
#define TEST136 0
#endif
int test136() {
    return TEST136;
}
// @end

// @try_ret test137 1
#if 0 == defined (TEST137_1)
#define TEST137 1
#else
#define TEST137 0
#endif
int test137() {
    return TEST137;
}
// @end

// @try_ret test138 1
#define TEST138_1
#ifdef TEST138_1
#define TEST138 1
#else
#define TEST138 0
#endif
int test138() {
    return TEST138;
}
// @end

// @try_ret test139 1
typedef int test139_a;
int test139_f(int test139_a) {
    return test139_a;
}

int test139() {
    test139_f(1);
}
// @end

// @try_ret test140 40
extern int i140;

int test140() {
    return i140;
}
// @end

// @try_ret test141 41
int f141();

int test141() {
    return 41;
}
// @end

// @try_ret test142 2
int test142() {
    int x = 2;
    return (char)x;
}
// @end

// @try_out test143 04030201
int test143() {
    char c[] = {1,2,3,4};
    try_printf("%08x\n", *((int*)c));
}
// @end

// @try_ret test144 44
int f144() { return 44; }
int test144() {
    return (*************************************************************************f144)();
}
// @end

// @try_ret test145 45
int test145() {
    int i = 40;
    i += 5;
    return i;
}
// @end

// @try_out test146 1-1
int test146() {
    int i = 0;
    int x = ++i;
    try_printf("%d-%d\n", x, i);
}
// @end

// @try_out test147 9-9
int test147() {
    int i = 10;
    int x = --i;
    try_printf("%d-%d\n", x, i);
}
// @end

// @try_out test148 0-1
int test148() {
    int i = 0;
    int x = i++;
    try_printf("%d-%d\n", x, i);
}
// @end

// @try_out test149 10-9
int test149() {
    int i = 10;
    int x = i--;
    try_printf("%d-%d\n", x, i);
}
// @end

// @try_ret test150 3
int test150() {
    return 150 % 7;
}
// @end

// @try_out test151 -1
int test151() {
    try_printf("%d\n", (-151) % 3);
}
// @end

// @try_out test152 5-5
int test152() {
    int i = 152;
    try_printf("%d-", i %= 7);
    try_printf("%d\n", i);
}
// @end

// @try_ret test153 2
int test153() {
    return 10 & 6;
}
// @end

// @try_out test154 2-2
int test154() {
    int i = 154;
    try_printf("%d-", i &= 7);
    try_printf("%d\n", i);
}
// @end

// @try_ret test155 14
int test155() {
    return 10 | 6;
}
// @end

// @try_out test156 14-14
int test156() {
    int i = 10;
    try_printf("%d-", i |= 6);
    try_printf("%d\n", i);
}
// @end

// @try_ret test157 12
int test157() {
    return 10 ^ 6;
}
// @end

// @try_out test158 12-12
int test158() {
    int i = 10;
    try_printf("%d-", i ^= 6);
    try_printf("%d\n", i);
}
// @end

// @try_out test159 0-then-159
int then159() {
    try_printf("then-");
    return 159;
}
int test159() {
    try_printf("%d-", 0 && then159());
    try_printf("%d\n", 1 && then159());
}
// @end

// @try_out test160 else-160-1
int else160() {
    try_printf("else-");
    return 160;
}
int test160() {
    try_printf("%d-", 0 || else160());
    try_printf("%d\n", 1 || else160());
}
// @end

// @try_ret test161 2
int test161() {
    return 1? 2: 3;
}
// @end

// @try_ret test162 3
int test162() {
    return 0? 2: 3;
}
// @end

// @try_ret test163 40
int test163() {
    return 5 << 3;
}
// @end

// @try_ret test164 3
int test164() {
    return 100 >> 5;
}
// @end

// @try_out test165 -4
int test165() {
    try_printf("%d", (-100) >> 5);
}
// @end

// @try_out test166 0
int test166() {
    try_printf("%d", !1);
}
// @end

// @try_out test167 1
int test167() {
    try_printf("%d", !0);
}
// @end

// @try_out test168 fffffffffffffffd
int test168() {
    try_printf("%lx", ~2);
}
// @end

// @try_ret test169 4
int test169() {
    return sizeof(int);
}
// @end

// @try_ret test170 70
int test170() {
    return (2,3,70);
}
// @end

// @try_out test171 -0-1-2-4
int test171() {
    int i;
    for (i = 0; ; i++) {
        if (i == 3) continue;
        if (i == 5) break;
        try_printf("-%d", i);
    }
}
// @end

// @try_out test172 -1-2-4
int test172() {
    int i = 0;
    while (1) {
        i++;
        if (i == 3) continue;
        if (i == 5) break;
        try_printf("-%d", i);
    }
}
// @end

// @try_out test173 abbcc
int test173() {
    int i;
    for (i = 0; i < 5; i++) {
        switch (i) {
            case 0:
                try_printf("a");
                break;
            case 1:
            case 2:
                try_printf("b");
                break;
            default:
                try_printf("c");
                break;
        }
    }
}
// @end

// @try_out test174 abb
int test174() {
    int i;
    for (i = 0; i < 5; i++) {
        switch (i) {
            case 0:
                try_printf("a");
                break;
            case 1:
            case 2:
                try_printf("b");
                break;
            default:
        }
    }
}
// @end

// @try_out test175 10
int test175() {
    void *p;
    int x = 10;
    p = &x;
    try_printf("%d", *(int*)p);
}
// @end

// @try_out test176 61,0a,5c,27,00,01,1b,23,
int test176() {
    char c[] = { 'a', '\n', '\\', '\'', '\0', '\1', '\033', '\x23' };

    int i;
    for (i = 0; i < sizeof c; i++ ) {
        try_printf("%02x,", c[i]);
    }
}
// @end

// @try_ret test177 100
static int f177() {
    return 100;
}

int test177() {
    return f177();
}
// @end

// @try_ret test178 100
static int x178;
int test178() {
    x178 = 100;
    return x178;
}
// @end

// @try_ret test179 79
int f179(int x,...) {
    return x;
}
int test179() {
    return f179(79);
}
// @end

// @try_out test180 foo
char *f180() {
    return "foo";
}
int test180() {
    try_printf("%s", f180());
}
// @end

// @try_out test181 0,1,2,
int test181() {
    for (int i = 0; i < 3; i++) {
        try_printf("%d,", i);
    }
}
// @end

// @try_out test182 foo-1-2-3-4-5-6-7-8
int f182(char *buf, char *fmt,...) {
    void *ap[4];

    __builtin_va_start(&ap);
    vsprintf(buf, fmt, &ap);
}
int test182() {
    char buf[256];

    f182(buf, "%s-%d-%d-%d-%d-%d-%d-%d-%d", "foo", 1, 2, 3, 4, 5, 6, 7, 8);
    try_printf("%s", buf);
}
// @end

// @try_out test183 2-3-0
struct { int p; int q; int r; } x183 = { 2, 3 };
int test183() {

    try_printf("%d-%d-%d", x183.p, x183.q, x183.r);
}
// @end

// @try_out test184 2-3-0
typedef struct { int p; int q; int r; } s184;
s184 *p184= &(s184){ 2, 3 };
int test184() {

    try_printf("%d-%d-%d", p184->p, p184->q, p184->r);
}
// @end

// @try_out test185 foo
int test185() {
    try_printf("foo");
    return;
}
// @end

// @try_out test186 foo
int test186() {
    do {
        try_printf("foo");
    } while (0);
}
// @end

// @try_out test187 2-3
int test187() {
    struct { int p; int q; int r; } x187 = { 2, 3 };

    try_printf("%d-%d", x187.p, x187.q);
}
// @end

// @try_out test188 2
int test188() {
    int count = 0;
    char s[] = "hogehoge";

    for (char *p = s; *p; p++)
        if (*p == 'o') count++;

    try_printf("%d", count);
}
// @end

// @try_out test189 foo
int test189() {
    char src[] = "foo";
    char dst[sizeof src];

    char *p;
    char *q;

    p = src;
    q = dst;
    while (*q++ = *p++);

    try_printf("%s", dst);
}
// @end

// @try_out test190 foo
void f190(int n) {
    for (int i = 0; i < n; i++) {
        printf("i=%d n=%d i<n=%d n>i=%d\n", i, n, i<n, n>i);
        try_printf("-%d", i);
    }
}
int test190() {
    try_printf("foo");
    f190(-10);
}
// @end

// @try_out test191 01111;00111;00011;00001;00000;
int test191() {
    for (int i = 0; i <= 4; i++) {
        for (int j = 0; j <= 4; j++) {
            int x = i - 2;
            int y = j - 2;
            int r;
            r = x < y;
            try_printf("%d", r);
        }
        try_printf(";");
    }
}
// @end

// @try_out test192 foo
int f192() {
    return 0;
}
int test192() {
    int flag = 1;

    if (flag && !f192())
        try_printf("foo");
}
// @end


// @try_out test193 100
int test193() {
    struct s1 { int x; } s1;
    struct s2 { struct s1 *s1; } s2;

    s1.x = 100;
    s2.s1 = &s1;

    try_printf("%d", s2.s1->x);

}
// @end
