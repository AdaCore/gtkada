@echo off

if "%1" == "" goto Usage

echo Generating Ada files...

info=`gate-in.exe -p -s %1 %2 %3 %4 %5 %6 %7 %8 %9`
set status=$?

if [ $status != 0 ]; then
  if [ $status = 2 ]; then
     echo $info
  fi

  echo "Couldn't parse $*. Exiting."
  exit 1
fi

set args = %1 %2 %3 %4 %5 %6 %7 %8 %9
set %info%
prj=$1

if [ "$2" != "" ]; then
  cd $2
fi

set gt = ".gate/$prj"
mkdir .gate > nul
mkdir %gt% > nul
set tmp = %gt%/tmp
del %tmp%\*.* > nul
mkdir %tmp%
set wd = `pwd`
gate-in.exe %args% > $tmp/gate.ada

if [ $? != 0 ]; then
  echo "Couldn't generate Ada code. Exiting."
  exit 1
fi

cd %tmp%
gnatchop gate.ada > nul
del gate.ada > nul
files=`echo *`
cd %wd%
del %gt%/gate.difs > nul

for j in $files; do
  diff -c $gt/$j $j >> $gt/gate.difs 2>/dev/null
done

copy %tmp%\*.* .
del *.rej *.orig

if cat %gt%/gate.difs | patch -f > $gt/patch.out 2>&1; then
  echo "The following files have been created/updated:"

  for j in $files; do
    echo "  "$j
  done

  echo done.
  /bin/rm -f *.orig
else
  echo "The following files have been updated:"

  for j in $files; do
    echo "  "$j
  done

  echo Merge of some changes failed. It usually means that some modified code
  echo is obsolete in the current project file.
  echo .rej files have been generated to help merging manually if needed.
fi

del %tmp%\*.*
del %gt%

goto End

:Usage
echo "Usage: gate project-file"

:End
