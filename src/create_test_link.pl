#! /usr/local/bin/perl

foreach (<gtk*.ads>)
{
    s/\.ads$//;                # remove the .ads extension
    s/-/\./;                   # change the dashes into dots
    s/(\w)(\w+)/\u$1$2/m;      # Uppercase the first letters of all words.
    s/_(\w)/_\u$1/;            # Uppercase the first letters after _

    print "with " . $_ . ";\n";    # output the result
}

print << "EOF"

procedure Test_Link is
begin
   null;
end Test_Link;
EOF
