SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .c .o .ads .adb .ali

.PHONY:
.PHONY: clean distclean mostlyclean maintainer-clean

target = testgtk

CC = gcc
CFLAGS = -O2 -g -Wall
GTKADA_CONFIG=../gtkada-config

ALL_CFLAGS = $(CFLAGS) `gtk-config --cflags`

GNATCC = gcc
GNATFLAGS = -gnatwu -g

GNATMAKE = gnatmake
GNATMAKEFLAGS = $(GNATFLAGS)
GNATMAKELDFLAGS = `gtk-config --libs` ../libgtkada.a
GNATMAKELDFLAGS_STATIC = `gtk-config --prefix`/lib/libgtk.a \
   `gtk-config --prefix`/lib/libglib.a \
   `gtk-config --libs | sed 's/-lglib//' | sed 's/-lgtk//'` ../libgtkada.a
#MALLOC_FLAGS= ../debug/wrap.o -Wl,--wrap=malloc -Wl,--wrap=free -Wl,--wrap=strdup
MALLOC_FLAGS=


all: $(target)

$(target): ../*.ad[sb] *.ad[sb]
	@echo "-------------------------------------"
	@echo $(GNATMAKE) $(GNATFLAGS) $(target) '`'$(GTKADA_CONFIG)'`'
	@echo "-------------------------------------"
	@$(GNATMAKE) $(GNATFLAGS) $(target) `$(GTKADA_CONFIG)`

static: ../*.ad[sb] *.ad[sb]
	$(GNATMAKE) $(GNATMAKEFLAGS) $(target) -largs $(MALLOC_FLAGS) $(GNATMAKELDFLAGS_STATIC)


install:
	@echo "Nothing to do --  just run the ./testgtk program"

clean:
	-${RM} *.o *.ali *~ testgtk b_testgtk.c core
	-${RM} .\#*

distclean: clean

mostlyclean: clean

maintainer-clean: clean


.adb.o:
	$(CC) -c $(GNATFLAGS) $<
.ads.o:
	$(CC) -c $(GNATFLAGS) $<

