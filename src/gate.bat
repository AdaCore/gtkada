@echo off
gate-in.exe %1 > all.ada
gnatchop -w all.ada
del all.ada
