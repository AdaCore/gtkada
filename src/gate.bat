@echo off

if "%1" == "" goto Error
goto Start

:Error
echo "Usage: gate project-file"
goto End

:Start
echo Generating Ada files...

gate-in.exe %1 > _tmp_.ada
gnatchop -w _tmp_.ada
del _tmp_.ada > nul

:End
