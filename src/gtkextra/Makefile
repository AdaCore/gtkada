SRC=gtkplot.c gtkplotcanvas.c gtkplotlayout.c gtkplotprint.c gtkplotps.c gtkpsfont.c gtkcheckitem.c gtkplotpc.c

OBJ=${SRC:%.c=%.o}
CC=gcc

all: ${OBJ}

%.o: %.c
	${CC} -c $< `gtk-config --cflags`

clean:
	${RM} ${OBJ}

