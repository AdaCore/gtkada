#! /usr/local/bin/perl

foreach (<g*.ads>)
{
    if (/g(lib|dk|tk)/)
    {
       open (FILE, $_);
       while ($line = <FILE>)
       {
           last if ($line =~ /package\s+(\S+)/);
       }
       close (FILE);
       $line =~ /package\s+(\S+)/;
       print "with $1;\n";
    }
}

print << "EOF"

procedure Test_Link is
begin
   null;
end Test_Link;
EOF
