TARGET= main
CC= gcc
CFLAGS= -c -g -Wall `gtk-config --cflags`

GNATMAKE= gnatmake
GNATFLAGS= -g -gnatg
GNATLARGS= `gtk-config --libs` $(OBJECTS)

OBJECTS= misc.o misc-gnat.o

$(TARGET):: $(OBJECTS) *.ad[sb]
	$(GNATMAKE) $(TARGET) $(GNATFLAGS) -largs $(GNATLARGS)


misc.o: misc.c misc-manu.c misc-joel.c

.c.o:
	$(CC) $(CFLAGS) $<

clean:
	-${RM} *.o *.ali *~ main b_main.c
	-${RM} test_link.adb test_link b_test_link.c
test_link: *.ad[sb] $(OBJECTS)
	./create_test_link.pl > test_link.adb
	$(GNATMAKE) test_link $(GNATFLAGS) -largs $(GNATLARGS)

