CC= gcc
CFLAGS= -Wall -c -g

GNATMAKE= gnatmake 
GNATFLAGS= -g -gnatg -I..

OBJECTS= misc.o misc-gnat.o

TARGET= testgtk

$(TARGET): $(OBJECTS) ../*.ad[sb] *.ad[sb]
	$(GNATMAKE) $(TARGET) $(GNATFLAGS) -largs `gtk-config --libs` $(OBJECTS)


misc.o: ../misc.c ../misc-joel.c ../misc-manu.c
	$(CC) $(CFLAGS) `gtk-config --cflags` ../misc.c

misc-gnat.o: ../misc-gnat.c
	$(CC) $(CFLAGS) `gtk-config --cflags` ../misc-gnat.c

clean:
	${RM} *.o *.ali *~ testgtk b_testgtk.c

