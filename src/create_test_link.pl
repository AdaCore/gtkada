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
       push (@list, $1);
       print "with $1;\n";
       print "pragma Warnings (off, $1);\n";
    }
}

print "procedure Test_Link is\n";
#foreach (@list)
#{
#   s/\./_/;
#   print "Widget_$_ : $_;\n";
#} 
print << "EOF";
begin
   null;
end Test_Link;
EOF
