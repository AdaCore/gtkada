#!/bin/sh

module=${1:-}
prefix=$2
default_library_type="$3"
version=$4
subdir=$5

lcmodule=`echo $module | tr [A-Z] [a-z]`

if [ "$OS" = "Windows_NT" ]; then
lcversion="-$version"
else
lcversion=""
fi

#### Create the linker page

echo_linker() {
   shared=$1

   if [ $shared = 1 ]; then
      libs=`sh ../src/tools/gtkada-config --libs`
   else
      libs=`sh ../src/tools/gtkada-config --static --libs`
   fi

   if [ "$lcmodule" = "gtkada" ]; then
      previous=""
      for lib in $libs; do
         if [ "$lib" != "-lgtkada" ]; then
            if [ x"$previous" != x"" ]; then
                echo "        \"$previous\","
            fi
            previous="$lib"
         fi
      done

      case `uname` in
         *_NT*)
            echo "        \"$previous\","
            echo "        \"-luser32\","
            echo "        \"-lglu32\","
            echo "        \"-lopengl32\","
            echo "        \"-lgdi32\","
	    if [ "$shared" = "1" ]; then
		echo "        \"-L../../bin\","
	    fi
	    echo "        \"-L..\","
	    echo -n "        \"-L../../include/gtkada/$subdir\""
            ;;
         *)
            if [ x"$previous" != x"" ]; then
              echo "        \"$previous\""
            fi
         ;;
      esac

   # Do nothing: since we are using library project files, these parameters
   # are set automatically
   #else
   #   if [ $shared = 1 ]; then
   #      echo "        \"-l${lcmodule}\""
   #   else
   #      echo "        \"$prefix/lib/static/lib${lcmodule}.a\""
   #   fi
   fi
}


#### Generate the project file

generate_shared() {
  uc=$1   # upper-case name
  lc=$2   # lower-case name
  default=$3

  cat <<EOF > ${lc}.gpr
project ${uc} is

   type Gtkada_Kind_Type is ("static", "relocatable");
   Gtkada_Kind : Gtkada_Kind_Type := external ("LIBRARY_TYPE", "$3");

   for Source_Dirs use ("../../include/gtkada/$subdir");
   for Library_Kind use Gtkada_Kind;

EOF

   case `uname` in
     *_NT*)
       cat <<EOF >> ${lc}.gpr
   case Gtkada_Kind is
      when "static" =>
         for Library_Name use "${lcmodule}";
         for Library_Dir use "../gtkada/static/$subdir";
      when "relocatable" =>
         for Library_Name use "${lcmodule}${lcversion}";
         for Library_Dir use "../../bin";
         for Library_ALI_Dir use "../gtkada/relocatable/$subdir";
   end case;

EOF
       ;;
     *)
      cat <<EOF >> ${lc}.gpr
   case Gtkada_Kind is
      when "static" =>
         for Library_Name use "${lcmodule}";
      when "relocatable" =>
         for Library_Name use "${lcmodule}${lcversion}";
   end case;

   for Library_Dir use "../gtkada/" & Project'Library_Kind;

EOF
   esac

   cat <<EOF >> ${lc}.gpr
   case Gtkada_Kind is
      when "static" =>
         for Library_Options use (
EOF
  echo_linker 0 >> ${lc}.gpr
  cat <<EOF >> ${lc}.gpr
          );
      when "relocatable" =>
          for Library_Options use (
EOF
  echo_linker 1 >> ${lc}.gpr
  cat <<EOF >> ${lc}.gpr
          );
   end case;

   for Externally_Built use "true";

   package Linker is
     for Linker_Options use Project'Library_Options;
   end Linker;
end ${uc};
EOF
}

generate_shared ${module} ${lcmodule} "$default_library_type"
