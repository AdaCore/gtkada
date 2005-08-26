#!/bin/sh

module=${1:-}
prefix=$2
has_shared="$3"

libs=`../src/gtkada-config --libs`

lcmodule=`echo $module | tr A-Z a-z`

#### Generate the project file for the relocatable library

generate_shared() {
  uc=$1   # upper-case name
  lc=$2   # lower-case name

  cat <<EOF > ${lc}.gpr
project ${uc} is
   for Source_Dirs use ("../../include/gtkada");
   for Source_List_File use "${lcmodule}.lgpr";
   for Library_Dir use "../gtkada";
   for Library_Kind use "relocatable";
   for Library_Name use "${lcmodule}";
   for Externally_Built use "true";
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
   for Source_List_File use "${lcmodule}.lgpr";
   for Externally_Built use "true";
   for Object_Dir use "../gtkada";

   package Linker is
      for Linker_Options use (
EOF

if [ x$lcmodule = xgtkada ]; then
   cpt=0
   for j in $libs; do
      lib="$j"
      if [ x"$lib" = x"-lgtkada" ]; then
         lib="${prefix}/lib/libgtkada.a"
      fi

      if [ $cpt -eq 1 ]; then
         echo "," >> ${lcmodule}_static.gpr
      fi
      cpt=1

      echo -n "        \"$lib\"" >> ${lcmodule}_static.gpr
   done

   case `uname` in
      *_NT*)
         echo "," >> ${lcmodule}_static.gpr
         echo -n "         \"-luser32\"" >> ${lcmodule}_static.gpr
      ;;
   esac

else
   echo -n "        \"$prefix/lib/lib${lcmodule}.a\"" >> ${lcmodule}_static.gpr
fi

   cat <<EOF >> ${lcmodule}_static.gpr
);
   end Linker;
end ${uc};
EOF


}

generate_static ${module}_Static ${lcmodule}_static

if [ x"$has_shared" != x ]; then
  generate_shared ${module}_Relocatable ${lcmodule}_relocatable
  generate_shared ${module} ${lcmodule}
else
  generate_static ${module} ${lcmodule}
fi

