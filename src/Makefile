TARGET= main
CC= gcc
CFLAGS= -O2 -c -g -Wall `gtk-config --cflags`

GNATMAKE= gnatmake
GNATFLAGS= -g -gnatg -O2 
GNATLARGS= `gtk-config --libs` $(OBJECTS) -lgtkada
AR=ar
ARFLAGS=rc
RANLIB=ranlib

C_OBJECTS= misc.o misc-gnat.o 

libgtkada.so: test_link.o $(C_OBJECTS)
	$(CC) -shared -o libgtkada.so g*.o $(C_OBJECTS)

libgtkada.a: test_link.o $(C_OBJECTS)
	$(AR) $(ARFLAGS) libgtkada.a g*.o $(C_OBJECTS)
	if [ -f /usr/bin/ranlib -o -f /bin/ranlib ]; then \
	  $(RANLIB) libgtkada.a; \
	fi 

main: main.adb test.adb test.ads libgtkada.so
	gnatmake main.adb $(GNATFLAGS) -largs $(GNATLARGS) 

install: libgtkada.a
	@echo To Install, copy libgtkada.a *.ads *.ali to a new directory
	@echo and then do a 'chmod a-w *.ali' so that Gnat will not
	@echo try to recompile your files

.c.o:
	$(CC) $(CFLAGS) $< -shared -fPIC
.adb.o:
	$(CC) -c $(GNATFLAGS) $<
.ads.o:
	$(CC) -c $(GNATFLAGS) $<

misc.o: misc.c misc-manu.c misc-joel.c

clean:
	-${RM} *.o *.ali *~ main b_main.c core
	-${RM} test_link b_test_link.c libgtkada.a libgtkada.so

test_link.o:: 
	#perl ./create_test_link.pl > test_link.adb
	$(GNATMAKE) -c test_link -shared -fPIC $(GNATFLAGS)
	$(RM) test_link.o 


