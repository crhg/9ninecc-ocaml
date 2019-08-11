#!/usr/bin/perl
#
sub string_escape {
    my($s) = @_;

    $s =~ s/\\/\\\\/g;
    $s =~ s/"/\\"/g;
    return $s;
}

sub gen_print_line {
    my($line) = @_;
    chomp $line;

    printf "    printf(\"%%s\\n\", \"%s\");\n", string_escape $line;
}

sub gen_print_code {
    my($line) = @_;

    gen_print_line $line;
    while ($line = <>) {
        gen_print_line $line;

        return if $line =~ m%^//\s+\@end%;
    }
}

printf("#include \"try.h\"\n");
printf("\n");
printf("void main(int argc, char **argv) {\n");

while (<>) {
    next unless m%^//\s%;
    chomp;

    my($comment, $try, $func, $expected) = split(/\s+/, $_, 4);

    # printf("// try=%s, func=%s, expected=%s\n", $try, $func, $expected);


    if ($try eq '@try_ret') {
        gen_print_code $_;

        printf("extern int %s();\n", $func);
        printf("try_ret(%s, %s(), \"%s\");\n", $expected, $func, $func);
    } elsif ($try eq '@try_out') {
        gen_print_code $_;

        unless ($expected =~ /".*"/) {
            $expected = '"'.$expected.'"';
        }

        printf("extern int %s();\n", $func);
        printf("try_out_start();\n");
        printf("%s();\n", $func);
        printf("try_out_check(%s, \"%s\");\n", $expected, $func);
    } else {
        # printf("// skip %s\n", $_);
    }

    printf("printf(\"\\n\");\n");
    printf("\n");
}

printf("printf(\"OK\\n\");\n");
printf("exit(0);\n");
printf("}\n");
