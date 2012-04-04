include ../src/Makefile.common

MKDIR = mkdir -p

SOVER=${MAJOR}.${MINOR}

prjdir="${DESTDIR}${prefix}/lib/gnat"

install: force
	${MKDIR} ${prjdir}
	$(MKDIR) "${DESTDIR}$(libdir)/pkgconfig/"
	${CP} gtkada.pc "${DESTDIR}$(libdir)/pkgconfig/"
	-${RM} "${DESTDIR}${prefix}"/projects/gtkada*

	./generate_prj.sh GtkAda "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${DESTDIR}${libdir}"
	${CP} gtkada.gpr ${prjdir}

ifeq ($(HAVE_LIBGLADE), True)
	./generate_prj.sh GtkAda_Glade "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${DESTDIR}${libdir}"
	${CP} gtkada_glade.gpr ${prjdir}
endif	

ifeq ($(HAVE_OPENGL), True)
	./generate_prj.sh GtkAda_GL "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} opengl "${DESTDIR}${libdir}"
	${CP} gtkada_gl.gpr ${prjdir}
endif	

ifeq ($(HAVE_GNOME), True)
	./generate_prj.sh GtkAda_Gnome "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${DESTDIR}${libdir}"
	${CP} gtkada_gnome.gpr ${prjdir}
endif	

ifeq ($(OS),Windows_NT)
	${CP} gtkada_install_*.gpr "${DESTDIR}${incdir}"
endif

clean: force
	${RM} *.gpr 

force:
