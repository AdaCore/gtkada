all:
	gnatmake -g main `/home/briot/gtk/install/bin/gtkada-config`
run:
	LD_LIBRARY_PATH=/home/briot/gtk/install/lib ./main

clean: 
	${RM} main *.o *.ali b_main.c

