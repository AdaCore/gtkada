EXEC=contextual banner toggle glist_traverse

all: ${EXEC}

${EXEC}::
	gnatmake -g $@ `gtkada-config`

clean::
	${RM} *.o *.ali b~* ${EXEC}

