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
target = testgtk

CC = gcc
CFLAGS = -O2 -g -Wall
ALL_CFLAGS = $(CFLAGS) `gtk-config --cflags`

GNATCC = gcc
GNATFLAGS = -O2 -g 
ALL_GNATFLAGS = $(GNATFLAGS) -I..

GNATMAKE = gnatmake
GNATMAKEFLAGS = $(ALL_GNATFLAGS)
GNATMAKELDFLAGS = `gtk-config --libs` ../libgtkada.a
GNATMAKELDFLAGS = -L/nile.d/briot/gtk/lib /nile.d/briot/gtk/lib/libgtk.a -lgtk -lgdk -lglib -lXext -lX11 -lsocket -lnsl -lm ../libgtkada.a

all: $(target)

$(target): ../*.ad[sb] *.ad[sb]
	$(GNATMAKE) $(GNATMAKEFLAGS) $(target) -largs $(GNATMAKELDFLAGS)


install: ;

uninstall: ;

install-strip: ;

clean:
	${RM} *.o *.ali *~ testgtk b_testgtk.c core

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

.adb.o:
	$(CC) -c $(ALL_GNATFLAGS) $<
.ads.o:
	$(CC) -c $(ALL_GNATFLAGS) $<

misc.o: ../misc.c ../misc-joel.c ../misc-manu.c
	$(CC) -c $(ALL_CFLAGS) ../misc.c

misc-gnat.o: ../misc-gnat.c
	$(CC) -c $(ALL_CFLAGS) ../misc-gnat.c
