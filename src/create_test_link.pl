#! /usr/local/bin/perl

open (VERSION, "gcc -gnatv gtk.ads 2> /dev/null | ");
$version=join (" ", <VERSION>);
close (VERSION);
$create_warnings = ($version =~ /3\.11w/);

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
       print "pragma Warnings (off, $1);\n" if ($create_warnings);
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
