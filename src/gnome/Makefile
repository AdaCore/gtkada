include ../Makefile.common

LIBNAME       = libgnomeada$(SO_EXT)
SONAME        = libgnomeada-$(MAJOR).$(MINOR)$(SO_EXT).$(MICRO)
ARCHIVENAME   = libgnomeada.a
GNOME_LIBS    = -lgnomeui -lgnome -lart_lgpl -lpopt

all: $(ARCHIVENAME) $(LIBNAME)

gnome:
	@if [ -f ../.devel ]; then \
	  $(GNATMAKE) -c $(GNATFLAGS_DEVEL) $(FPIC) -I.. -aO../lib-obj make; \
	else \
	  $(GNATMAKE) -c $(GNATFLAGS) $(FPIC) -I.. make; \
	fi

$(LIBNAME): gnome
ifeq ($(BUILD_SHARED),yes)
	  $(CC) -shared $(FPIC) $(OS_SPECIFIC_LINK_OPTIONS) -o $(SONAME) \
	  $(SO_OPTS)$(SONAME) gnome.o gnome-*.o $(GTK_LIBS)
	  $(RM) $(LIBNAME)
	  $(LN) $(SONAME) $(LIBNAME)
	  -@if [ ! -f ../.devel ]; then \
	    strip $(LIBNAME); \
	    $(CHMOD) +x $(LIBNAME); \
	  fi
endif

$(ARCHIVENAME): gnome
	$(AR) $(ARFLAGS) $(ARCHIVENAME) gnome.o gnome-*.o
	if [ -f /usr/bin/$(RANLIB) -o -f /bin/$(RANLIB) ]; then \
	  $(RANLIB) $(ARCHIVENAME); \
	fi

link:
	gnatmake -gnaty make `gtkada-config` -largs $(GNOME_LIBS)

clean:
	$(RM) *.o *.ali *~ core
