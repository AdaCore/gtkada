EXEC=contextual

all: ${EXEC}

contextual::
	gnatmake -g contextual `gtkada-config`

clean::
	${RM} *.o *.ali b~* ${EXEC}

