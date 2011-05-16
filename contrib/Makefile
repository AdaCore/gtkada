
#GIR_FILES=/usr/share/gir-1.0/Gtk-2.0.gir
GIR_FILES=Gtk-2.0.gir
all:${GIR_FILES}

${GIR_FILES}: force
	rm -f generated/*.ad?
	python ./binding.py $@ ./binding.xml
	cd generated; gnatchop -w tmp.ada
	cd generated; gnatmake -p -Pdefault -U -c

force:
