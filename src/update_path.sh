#
prefix=`pwd`
sed -e "s'PREFIX'$prefix'" gate.bat > gate.tmp
cp gate.tmp gate.bat
rm gate.tmp
