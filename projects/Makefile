include ../src/Makefile.common

MKDIR = mkdir -p

SOVER=${MAJOR}.${MINOR}

prjdir="${DESTDIR}${prefix}/lib/gnat"

install: force
	${MKDIR} ${prjdir}
	$(MKDIR) "${DESTDIR}$(libdir)/pkgconfig/"
	${CP} gtkada.pc "${DESTDIR}$(libdir)/pkgconfig/"
	-${RM} "${DESTDIR}${prefix}"/projects/gtkada*

	@# We must pass ${libdir} and not ${DESTDIR}${libdir}, since DESTDIR is
	@# really only a temporary directory for packagers, but the package will
	@# eventually be installed in ${libdir}.
	./generate_prj.sh GtkAda "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${libdir}"
	${CP} gtkada.gpr ${prjdir}

ifeq ($(HAVE_LIBGLADE), True)
	./generate_prj.sh GtkAda_Glade "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${libdir}"
	${CP} gtkada_glade.gpr ${prjdir}
endif	

ifeq ($(HAVE_OPENGL), True)
	./generate_prj.sh GtkAda_GL "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} opengl "${libdir}"
	${CP} gtkada_gl.gpr ${prjdir}
endif	

ifeq ($(HAVE_GNOME), True)
	./generate_prj.sh GtkAda_Gnome "${DESTDIR}${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} "" "${libdir}"
	${CP} gtkada_gnome.gpr ${prjdir}
endif	

ifeq ($(OS),Windows_NT)
	${CP} gtkada_install_*.gpr "${DESTDIR}${incdir}"
endif

clean: force
	${RM} *.gpr 

force:
