#

if [ $# -eq 0 ]; then
  echo "Usage: gate project-file" | gdialog error justify_left
  exit 1
fi

dir=`dirname $1`
file=$1
info=`gate-in.exe -p -s -x $file`
status=$?

if [ $status != 0 ]; then
  echo "Couldn't parse $file. Exiting." | gdialog error justify_left
  exit 1
fi

set $info

if [ "$1" = "<no_name>" ]; then
   prj=default
else
   prj=$1
fi

if [ "$2" = "<no_name>" ]; then
   srcdir=.
   psrcdir="the current directory"
else
   srcdir=$2
   psrcdir=$srcdir
fi

pixdir="$3"
# Copy any pixmap files from pixdir to srcdir

mkdir -p $dir/$srcdir

if [ "$pixdir" != "<no_name>" -a -d $dir/$pixdir ]; then
   cp $dir/$pixdir/*xpm $dir/$srcdir > /dev/null 2>&1
fi

owd=`pwd`
cd $dir/$srcdir

if [ $? != 0 ]; then
  echo "Couldn't change to $dir/$srcdir, aborting." | gdialog error justify_left
  exit 1
fi

gt=".gate/$prj"
mkdir -p $gt > /dev/null 2>&1
tmp=$gt/tmp
rm -rf $tmp
mkdir $tmp
wd=`pwd`
out=$gt/output.txt

gate-in.exe $file > $tmp/gate.ada

if [ $? != 0 ]; then
  echo "Couldn't generate Ada code. Exiting." | gdialog error justify_left
  exit 1
fi

cd $tmp
gnatchop gate.ada > /dev/null 2>&1
rm -f gate.ada
files=`echo *`
cd $wd
rm -f $gt/gate.difs

for j in $files; do
  diff -u $gt/$j $j >> $gt/gate.difs 2>/dev/null
done

cp -f $tmp/* .
rm -f *.rej *.orig

if cat $gt/gate.difs | patch -f > $gt/patch.out 2>&1; then
  echo "The following files have been created/updated in $psrcdir:" > $out

  for j in $files; do
    echo "  "$j >> $out
  done

  rm -f *.orig
  # cat $out | gdialog information justify_fill
else
  echo "The following files have been updated in $psrcdir:" > $out

  for j in $files; do
    echo "  "$j >> $out
  done

  echo Merge of some changes failed. It usually means that some modified>> $out
  echo code is obsolete in the current project file.>> $out
  echo Files with the ".rej" extension have been generated to help merging>>$out
  echo manually if needed.>> $out

  cat $out | gdialog error justify_fill
fi

cp -f $tmp/* $gt
