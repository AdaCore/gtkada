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

all: libs gtkada-config testgtk 

libs:: libgtkada.so libgtkada.a

libgtkada.so:: $(TEST_LINK).o $(c_objects)
	$(CC) -shared -Wl,-soname,libgtkada.so -o libgtkada.so g*.o $(c_objects) `gtk-config --libs` -lm

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

main: main.adb test.adb test.ads libgtkada.so
	$(GNATMAKE) $@ $(GNATMAKEFLAGS) $(GNATMAKELDFLAGS) 

install: libgtkada.a libgtkada.so gtkada-config
	@if [ "$(prefix)" != `pwd` ]; then \
	   @echo Installing GtkAda in $(prefix);            \
	   $(RM) test_link.ali;                             \
	   $(INSTALL) -d $(prefix);                         \
	   $(INSTALL) libgtkada.a libgtkada.so $(prefix);   \
	   $(INSTALL) --mode=a-w+r *.ads $(prefix);         \
	   $(INSTALL) --mode=a-w+r *.ali $(prefix);         \
	   $(INSTALL) --mode=a+rx gtkada-config $(prefix);  \
	   @echo "All the files have been copied.";         \
	fi
	@echo You may want to keep a copy of gtkada-config somewhere in your PATH

gtkada-config: gtkada-config.in Makefile
	@echo "Building the gtkada-config script"
	@cat gtkada-config.in | sed -e 's#@prefix@#'$(prefix)'#' \
	  -e 's#@gtkada_cflags@#-I'$(prefix)'#' \
	  -e 's#@gtkada_libs@#-L'$(prefix)'#'  > ${@}
	@chmod a+x ${@}

clean:
	-${RM} *.o *.ali *~ main b_main.c core
	-${RM} libgtkada.a libgtkada.so
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


