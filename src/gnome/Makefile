include ../Makefile.common

all:
	@if [ -f ../.devel ]; then \
	  $(GNATMAKE) -c $(GNATFLAGS_DEVEL) -I.. -aO../lib-obj make; \
	else \
	  $(GNATMAKE) -c $(GNATFLAGS) -I.. make; \
	fi

link:
	gnatmake -gnaty make `gtkada-config` -largs -lgnomeui -lgnome -lart_lgpl -lpopt

clean:
	$(RM) *.o *.ali *~ core
