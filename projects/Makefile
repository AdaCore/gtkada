include ../src/Makefile.common

MKDIR = mkdir -p

SOVER=${MAJOR}.${MINOR}

install: force
	${MKDIR} "${DESTDIR}$(libdir)/gnat/"
	$(MKDIR) "${DESTDIR}$(libdir)/pkgconfig/"
	${CP} gtkada.pc "${DESTDIR}$(libdir)/pkgconfig/"
	-${RM} "${DESTDIR}${prefix}"/projects/gtkada*

	./generate_prj.sh GtkAda "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${DESTDIR}${libdir}"
	${CP} gtkada.gpr "${DESTDIR}${libdir}/gnat/"

ifeq ($(HAVE_LIBGLADE), True)
	./generate_prj.sh GtkAda_Glade "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${DESTDIR}${libdir}"
	${CP} gtkada_glade.gpr ${libdir}/gnat/
endif	

ifeq ($(HAVE_OPENGL), True)
	./generate_prj.sh GtkAda_GL "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} opengl "${DESTDIR}${libdir}"
	${CP} gtkada_gl.gpr "${DESTDIR}${libdir}/gnat/"
endif	

ifeq ($(HAVE_GNOME), True)
	./generate_prj.sh GtkAda_Gnome "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${DESTDIR}${libdir}"
	${CP} gtkada_gnome.gpr "${DESTDIR}${libdir}/gnat/"
endif	

ifeq ($(OS),Windows_NT)
	${CP} gtkada_install_*.gpr "${DESTDIR}${incdir}"
endif

clean: force
	${RM} *.gpr 

force:
