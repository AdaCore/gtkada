SHELL = /bin/sh

# INSTALL = install
# INSTALL_PROGRAM = $(INSTALL)
# INSTALL_DATA = $(INSTALL)

prefix = /usr/local
exec_prefix = $(prefix)
libdir = $(exec_prefix)/lib
infodir = $(prefix)/info
mandir = $(prefix)/man
man1dir = $(mandir)/man1
man3dir = $(mandir)man3
man1ext = .1
man3ext = .3

.SUFFIXES:
.SUFFIXES: .c .o .ads .adb .ali

.PHONY:
.PHONY: clean distclean mostlyclean maintainer-clean

c_objects = misc.o misc-gnat.o

TEST_LINK = test_link

CC = gcc
CFLAGS = -O2 -g -Wall
ALL_CFLAGS = $(CFLAGS) `gtk-config --cflags` -shared -fPIC

GNATCC = gcc
GNATFLAGS = -O2 -g -gnatg
ALL_GNATFLAGS = $(GNATFLAGS) -shared -fPIC

GNATMAKE = gnatmake
GNATMAKEFLAGS = $(GNATFLAGS)
GNATMAKELDFLAGS = -largs `gtk-config --libs` $(c_objects) -lgtkada

ARFLAGS = r

RANLIB = ranlib

all: libgtkada.so libgtkada.a

libgtkada.so:: test_link.o
	$(CC) -shared -o libgtkada.so g*.o $(c_objects)

libgtkada.a::
	$(AR) $(ARFLAGS) libgtkada.a g*.o $(c_objects)
	if [ -f /usr/bin/$(RANLIB) -o -f /bin/$(RANLIB) ]; then \
	  $(RANLIB) libgtkada.a; \
	fi \

test_link.o::
	#perl ./create_test_link.pl > test_link.adb
	$(GNATMAKE) -c test_link -shared -fPIC $(GNATFLAGS)

main: main.adb test.adb test.ads libgtkada.so
	$(GNATMAKE) $@ $(GNATMAKEFLAGS) $(GNATMAKELDFLAGS) 

install: libgtkada.a libgtkada.so
	@echo To Install, copy libgtkada.a *.ads *.ali to a new directory
	@echo and then do a 'chmod a-w *.ali' so that Gnat will not
	@echo try to recompile your files

uninstall:

install-strip:

clean:
	-${RM} *.o *.ali *~ main b_main.c core
	-${RM} test_link b_test_link.c libgtkada.a libgtkada.so
	-cd testgtk && make clean

distclean: clean

mostlyclean: clean

maintainer-clean: clean


TAGS: ;

info: ;

dvi: ;

dist: ;

check: ;

installcheck: ;

installdirs: ;

.c.o:
	$(CC) -c $(ALL_CFLAGS) $<
.ads.o:
	$(CC) -c $(ALL_GNATFLAGS) $<

.adb.o:
	$(CC) -c $(ALL_GNATFLAGS) $<

misc.o: misc.c misc-manu.c misc-joel.c
