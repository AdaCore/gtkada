#! /usr/bin/env perl

$version=`gcc -gnatv gtk.ads 2> /dev/null | grep GNAT`;
$version =~ s/^GNAT (.\...).*$/$1/;

print "pragma Warnings (Off);\n" if $version >= '3.11';

foreach (<gtk*.ads>, <gdk*.ads>, <glib*.ads>)
{
    if (/g(lib|dk|tk)/)
    {
       open (FILE, $_);
       while ($line = <FILE>)
       {
           last if ($line =~ /^\s*package\s+(\S+)/);
       }
       close (FILE);
       $line =~ /package\s+(\S+)/;
       push (@list, $1);
       print "with $1;\n";
    }
}

print << "EOF";
procedure Test_Link is
begin
   null;
end Test_Link;
EOF
