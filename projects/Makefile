include ../src/Makefile.common

MKDIR = mkdir -p

SOVER=${MAJOR}.${MINOR}

install: force
	${MKDIR} "${DESTDIR}$(prefix)/lib/gnat/"
	$(MKDIR) "${DESTDIR}$(prefix)/lib/pkgconfig/"
	${CP} gtkada.pc "${DESTDIR}$(prefix)/lib/pkgconfig/"
	-${RM} "${DESTDIR}${prefix}"/projects/gtkada*
	./generate_prj.sh GtkAda "${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} ""
	${CP} gtkada.gpr "${DESTDIR}${prefix}/lib/gnat/"
ifeq ($(HAVE_LIBGLADE), True)
	./generate_prj.sh GtkAda_Glade "${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} ""
	${CP} gtkada_glade.gpr ${prefix}/lib/gnat/
endif	
ifeq ($(HAVE_OPENGL), True)
	./generate_prj.sh GtkAda_GL "${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} opengl
	${CP} gtkada_gl.gpr "${DESTDIR}${prefix}/lib/gnat/"
endif	
ifeq ($(HAVE_GNOME), True)
	./generate_prj.sh GtkAda_Gnome "${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER} ""
	${CP} gtkada_gnome.gpr "${DESTDIR}${prefix}/lib/gnat/"
endif	
ifeq ($(OS),Windows_NT)
	${CP} gtkada_install_*.gpr "${DESTDIR}${prefix}/include/gtkada"
endif

clean: force
	${RM} *.gpr 

force:
