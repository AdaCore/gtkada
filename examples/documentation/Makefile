EXEC=contextual banner toggle

all: ${EXEC}

${EXEC}::
	gnatmake -g $@ `gtkada-config`

clean::
	${RM} *.o *.ali b~* ${EXEC}

