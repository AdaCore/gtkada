include ../src/Makefile.common

MKDIR = mkdir -p

SOVER=${MAJOR}.${MINOR}

install: force
	${MKDIR} $(prefix)/lib/gnat/
	${MKDIR} $(prefix)/lib/gnat/gtkada
	$(MKDIR) $(prefix)/lib/pkgconfig/
	${CP} gtkada.pc $(prefix)/lib/pkgconfig/
	-${RM} ${prefix}/projects/gtkada*
	./generate_prj.sh GtkAda "${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER}
	${CP} gtkada.gpr ${prefix}/lib/gnat/
ifeq ($(HAVE_LIBGLADE), True)
	./generate_prj.sh GtkAda_Glade "${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER}
	${CP} gtkada_glade.gpr ${prefix}/lib/gnat/
endif	
ifeq ($(HAVE_OPENGL), True)
	./generate_prj.sh GtkAda_GL "${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER}
	${CP} gtkada_gl.gpr ${prefix}/lib/gnat/
endif	
ifeq ($(HAVE_GNOME), True)
	./generate_prj.sh GtkAda_Gnome "${prefix}" ${DEFAULT_LIBRARY_TYPE} ${SOVER}
	${CP} gtkada_gnome.gpr ${prefix}/lib/gnat/
endif	
	${CP} *.lgpr ${prefix}/lib/gnat/gtkada
ifeq ($(OS),Windows_NT)
	${CP} gtkada_install_*.gpr ${prefix}/include/gtkada
endif

clean: force
	${RM} *.gpr 

force:
