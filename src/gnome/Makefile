include ../Makefile.common

LIBNAME       = libgnomeada$(SO_EXT)
SONAME        = libgnomeada-$(MAJOR).$(MINOR)$(SO_EXT).$(MICRO)
ARCHIVENAME   = libgnomeada.a

all: $(ARCHIVENAME)

gnome:
	@if [ -f ../.devel ]; then \
	  $(GNATMAKE) -c $(GNATFLAGS_DEVEL) -I.. -aO../lib-obj make; \
	else \
	  $(GNATMAKE) -c $(GNATFLAGS) -I.. make; \
	fi

$(ARCHIVENAME): gnome
	$(AR) $(ARFLAGS) $(ARCHIVENAME) gnome.o gnome-*.o
	if [ -f /usr/bin/$(RANLIB) -o -f /bin/$(RANLIB) ]; then \
	  $(RANLIB) $(ARCHIVENAME); \
	fi

link:
	gnatmake -gnaty make `gtkada-config` -largs -lgnomeui -lgnome -lart_lgpl -lpopt

clean:
	$(RM) *.o *.ali *~ core
