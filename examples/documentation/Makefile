EXEC=contextual banner toggle glist_traverse tooltips clist draw

all: ${EXEC}

${EXEC}::
	gnatmake -g $@ `gtkada-config`

clean::
	${RM} *.o *.ali b~* ${EXEC}

