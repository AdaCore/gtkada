#!/bin/sh

module=${1:-}
prefix=$2
has_shared="$3"
version=$4

libs=`sh ../src/tools/gtkada-config --libs`

lcmodule=`echo $module | tr [A-Z] [a-z]`

if [ "$OS" = "Windows_NT" ]; then
lcversion="-$version"
else
lcversion=""
fi

#### Create the linker page

echo_linker() {
   shared=$1

   echo "   package Linker is"
   echo "      for Linker_Options use ("

   if [ "$lcmodule" = "gtkada" ]; then
      cpt=0
      for j in $libs; do
         lib="$j"
         if [ "$lib" != "-lgtkada" ]; then
            if [ $cpt -eq 1 ]; then
               echo ","
            fi
            cpt=1
            echo "        \"$lib\""
         fi
      done

      case `uname` in
         *_NT*)
            echo ","
            echo "        \"-luser32\","
	    if [ "$shared" = "1" ]; then
		echo "        \"-L../../bin\","
	    fi
	    echo "        \"-L..\","
	    echo -n "        \"-L../../include/gtkada\""
         ;;
      esac

   else
      echo "        \"$prefix/lib/lib${lcmodule}.a\""
   fi

   echo "      );"
   echo "   end Linker;"
}


#### Generate the project file for the relocatable library

generate_shared() {
  uc=$1   # upper-case name
  lc=$2   # lower-case name

  cat <<EOF > ${lc}.gpr
project ${uc} is
   for Source_Dirs use ("../../include/gtkada");
   for Source_List_File use "gtkada/${lcmodule}.lgpr";
   for Library_Dir use "../gtkada";
   for Library_Kind use "relocatable";
   for Library_Name use "${lcmodule}${lcversion}";
   for Externally_Built use "true";
EOF
  echo_linker 1 >> ${lc}.gpr
  cat <<EOF >> ${lc}.gpr
end ${uc};
EOF
}

#### Generate the project file for the static library

generate_static() {
  uc=$1
  lc=$2
  cat <<EOF > ${lc}.gpr
project ${uc} is
   for Source_Dirs use ("../../include/gtkada");
   for Source_List_File use "gtkada/${lcmodule}.lgpr";
   for Library_Dir use "../gtkada";
   for Library_Kind use "static";
   for Library_Name use "${lcmodule}";
   for Externally_Built use "true";
EOF
  echo_linker 0 >> ${lc}.gpr
  cat <<EOF >> ${lc}.gpr
end ${uc};
EOF
}

generate_static ${module}_Static ${lcmodule}_static

if [ x"$has_shared" = xyes ]; then
  generate_shared ${module}_Relocatable ${lcmodule}_relocatable
  generate_shared ${module} ${lcmodule}
else
  generate_static ${module} ${lcmodule}
fi
