
main: misc.o *.adb *.ads
	gnatmake main -g -gnatg -largs misc.o misc-gnat.o -lglib -lgdk -lgtk -lX11 -lXext -lm
clean::
	${RM} *.o *.ali *~ main b_main.c
.c.o:
	gcc -c -O2 $< -o $@ -I../include/ -I../lib/glib/include

