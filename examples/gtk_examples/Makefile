EXAMPLES=scribble

all: ${EXAMPLES}

scribble::
	gnatmake -g $@ `gtkada-config`
clean::
	${RM} *.o *.ali scribble b~*

