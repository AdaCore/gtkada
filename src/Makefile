SHELL=/bin/sh

#### Please modify the installation directory below
prefix=`pwd`



exec_prefix = $(prefix)

.SUFFIXES:
.SUFFIXES: .c .o .ads .adb .ali

.PHONY:
.PHONY: clean distclean mostlyclean maintainer-clean

c_objects = misc.o misc-gnat.o

TEST_LINK = test_link
INSTALL = install

CC = gcc
CFLAGS = -O2 -g -Wall
ALL_CFLAGS = $(CFLAGS) `gtk-config --cflags` -shared -fPIC

GNATCC = gcc
GNATFLAGS = -O2 -g -gnatg -gnata
ALL_GNATFLAGS = $(GNATFLAGS) -shared -fPIC

GNATMAKE = gnatmake
GNATMAKEFLAGS = $(GNATFLAGS)
GNATMAKELDFLAGS = -largs `gtk-config --libs` -lgtkada

ARFLAGS = r

RANLIB = ranlib

SONAME = libgtkada.so
LIBNAME = libgtkada.so

bindir = $(prefix)
libdir = $(prefix)
incdir = $(prefix)
alidir = $(prefix)

all: libs gtkada-config testgtk 

libs:: $(LIBNAME) libgtkada.a

$(LIBNAME):: $(TEST_LINK).o $(c_objects)
	$(CC) -shared -o $(LIBNAME) glib*.o gdk*.o gtk*.o $(c_objects) `gtk-config --libs` -lm

libgtkada.a:: $(TEST_LINK).o $(c_objects)
	$(AR) $(ARFLAGS) libgtkada.a g*.o $(c_objects)
	if [ -f /usr/bin/$(RANLIB) -o -f /bin/$(RANLIB) ]; then \
	  $(RANLIB) libgtkada.a; \
	fi

testgtk::
	$(MAKE) -C testgtk

$(TEST_LINK).o::
	test -f $(TEST_LINK).adb || perl ./create_test_link.pl > $(TEST_LINK).adb
	$(GNATMAKE) -c $(TEST_LINK) -shared -fPIC $(GNATFLAGS)

main: main.adb test.adb test.ads $(LIBNAME)
	$(GNATMAKE) $@ $(GNATMAKEFLAGS) $(GNATMAKELDFLAGS) 

install: libgtkada.a $(LIBNAME) gtkada-config
	@if [ "$(prefix)" != `pwd` ]; then \
	   echo Installing GtkAda in $(prefix);            \
	   $(RM) test_link.ali;                             \
	   $(INSTALL) -d $(bindir) $(libdir) $(incdir) $(alidir); \
	   $(INSTALL) libgtkada.a $(LIBNAME) $(libdir);   \
	   $(INSTALL) --mode=a-w+r *.ads *.adb $(incdir); \
	   $(INSTALL) --mode=a-w+r *.ali $(alidir);         \
	   $(INSTALL) --mode=a+rx gtkada-config $(bindir);  \
	   echo "All the files have been copied.";          \
	fi
	@echo You may want to keep a copy of gtkada-config somewhere in your PATH

gtkada-config: gtkada-config.in Makefile
	@echo "Building the gtkada-config script"
	@cat gtkada-config.in | sed -e 's#@prefix@#'$(prefix)'#' \
	  -e 's#@gtkada_cflags@#-I'$(incdir)'#' \
	  -e 's#@gtkada_libs@#-L'$(libdir)'#'  > ${@}
	@chmod a+x ${@}

clean:
	-${RM} *.o *.ali *~ main b_main.c core
	-${RM} libgtkada.a $(LIBNAME)
	-${RM} $(TEST_LINK) b_$(TEST_LINK).c $(TEST_LINK).adb
	-${RM} .\#*
	-cd testgtk && make clean

distclean: clean

mostlyclean: clean

maintainer-clean: clean

.c.o:
	$(CC) -c $(ALL_CFLAGS) $<
.ads.o:
	$(CC) -c $(ALL_GNATFLAGS) $<

.adb.o:
	$(CC) -c $(ALL_GNATFLAGS) $<


