#!/usr/bin/perl

my $only = (@ARGV) > 1 ? pop @ARGV : undef;

# printf "// only=%s\n", defined $only ? $only : 'undef';

my $out_flag = 1;
while (<>) {
    if (m%^//\s+\@try%) {

        my($comment, $try, $func, $expected) = split(/\s+/, $_, 4);

        # printf("// try=%s, func=%s, expected=%s\n", $try, $func, $expected);

        $out_flag = !defined $only || $func eq $only;
        # printf("// try: out_flag=%d\n", $out_flag);
    }

    print if $out_flag;

    if (m%^//\s+\@end%) {
        $out_flag = 1;
        # printf("// end: out_flag=%d\n", $out_flag);
    }
}
