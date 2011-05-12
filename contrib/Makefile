
GIR_FILES=/usr/share/gir-1.0/Gtk-2.0.gir

all:${GIR_FILES}

${GIR_FILES}: force
	python ./binding.py $@ ./binding.xml > generated/tmp.ada
	cd generated; gnatchop -w tmp.ada
	cd generated; gnatmake -p -Pdefault -u *.adb

force:
