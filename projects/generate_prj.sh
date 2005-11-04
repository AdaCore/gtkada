#!/bin/sh

module=${1:-}
prefix=$2
has_shared="$3"

libs=`../src/gtkada-config --libs`

lcmodule=`echo $module | tr A-Z a-z`

#### Create the linker page

echo_linker() {
   shared=$1

   echo "   package Linker is"
   echo "      for Linker_Options use ("

   if [ x$lcmodule = xgtkada ]; then
      cpt=0
      for j in $libs; do
         lib="$j"
         if [ x"$lib" = x"-lgtkada" ]; then
            lib=""
         fi

         if [ x"$lib" != x ]; then
            if [ $cpt -eq 1 ]; then
               echo ","
            fi
            cpt=1
            echo -n "        \"$lib\""
         fi
      done

      case `uname` in
         *_NT*)
            echo ","
            echo -n "        \"-luser32\""
         ;;
      esac

   else
      echo -n "        \"$prefix/lib/lib${lcmodule}.a\""
   fi

   echo ");"
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
   for Library_Name use "${lcmodule}";
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

