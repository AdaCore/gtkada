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

target = testgtk

CC = gcc
CFLAGS = -O2 -g -Wall
ALL_CFLAGS = $(CFLAGS) `gtk-config --cflags`

GNATCC = gcc
GNATFLAGS += -O2 -g
ALL_GNATFLAGS = $(GNATFLAGS) -I..

GNATMAKE = gnatmake
GNATMAKEFLAGS = $(ALL_GNATFLAGS)
GNATMAKELDFLAGS = `gtk-config --libs` ../libgtkada.a
GNATMAKELDFLAGS_STATIC = `gtk-config --prefix`/lib/libgtk.a \
   `gtk-config --prefix`/lib/libglib.a \
   `gtk-config --libs | sed 's/-lglib//' | sed 's/-lgtk//'` ../libgtkada.a
#MALLOC_FLAGS= ../debug/wrap.o -Wl,--wrap=malloc -Wl,--wrap=free -Wl,--wrap=strdup
MALLOC_FLAGS=


all: $(target)

$(target): ../*.ad[sb] *.ad[sb]
	$(GNATMAKE) $(GNATMAKEFLAGS) $(target) -largs $(GNATMAKELDFLAGS)

static: ../*.ad[sb] *.ad[sb]
	$(GNATMAKE) $(GNATMAKEFLAGS) $(target) -largs $(MALLOC_FLAGS) $(GNATMAKELDFLAGS_STATIC)


install: ;

uninstall: ;

install-strip: ;

clean:
	-${RM} *.o *.ali *~ testgtk b_testgtk.c core
	-${RM} .\#*

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
