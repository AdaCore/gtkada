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
GNATFLAGS = -O2 -g -gnatg -gnata
ALL_GNATFLAGS = $(GNATFLAGS) -shared -fPIC

GNATMAKE = gnatmake
GNATMAKEFLAGS = $(GNATFLAGS)
GNATMAKELDFLAGS = -largs `gtk-config --libs` $(c_objects) -lgtkada

ARFLAGS = r

RANLIB = ranlib

all: libgtkada.so libgtkada.a testgtk

libgtkada.so:: $(TEST_LINK).o $(c_objects)
	$(CC) -shared -Wl,-soname,libgtkada.so.0 -o libgtkada.so g*.o $(c_objects) `gtk-config --libs` -lc -lm

libgtkada.a::
	$(AR) $(ARFLAGS) libgtkada.a g*.o $(c_objects)
	if [ -f /usr/bin/$(RANLIB) -o -f /bin/$(RANLIB) ]; then \
	  $(RANLIB) libgtkada.a; \
	fi \

testgtk::
	$(MAKE) -C testgtk

$(TEST_LINK).o::
	test -f $(TEST_LINK).adb || perl ./create_test_link.pl > $(TEST_LINK).adb
	$(GNATMAKE) -c $(TEST_LINK) -shared -fPIC $(GNATFLAGS)

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
	-${RM} libgtkada.a libgtkada.so
	-${RM} $(TEST_LINK) b_$(TEST_LINK).c $(TEST_LINK).adb
	-${RM} .\#*
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
