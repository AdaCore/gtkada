#! /usr/local/bin/perl

foreach (<g*.ads>)
{
    if (/g(lib|dk|tk)/)
    {
	s/\.ads$//;                # remove the .ads extension
	s/-/\./g;                  # change the dashes into dots
	s/(\w+)/\u$1/g;            # Uppercase the first letters of all words.
	s/_(\w)/_\u$1/g;           # Uppercase the first letters after _

	print "with " . $_ . ";\n";    # output the result
    }
}

print << "EOF"

procedure Test_Link is
begin
   null;
end Test_Link;
EOF
