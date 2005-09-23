include ../src/Makefile.common

MKDIR = mkdir -p

install: force
	${MKDIR} $(prefix)/lib/gnat/
	${MKDIR} $(prefix)/lib/gnat/deps
	$(MKDIR) $(prefix)/lib/pkgconfig/
	${CP} gtkada.pc $(prefix)/lib/pkgconfig/
	-${RM} ${prefix}/projects/gtkada*
	./generate_prj.sh GtkAda "${prefix}" ${BUILD_SHARED}
ifeq ($(HAVE_LIBGLADE), True)
	./generate_prj.sh GtkAda_Glade "${prefix}" ${BUILD_SHARED}
endif	
ifeq ($(HAVE_OPENGL), True)
	./generate_prj.sh GtkAda_GL "${prefix}" ${BUILD_SHARED}
endif	
ifeq ($(HAVE_GNOME), True)
	./generate_prj.sh GtkAda_Gnome "${prefix}" ${BUILD_SHARED}
endif	
	${CP} *.gpr ${prefix}/lib/gnat/
	${CP} *.lgpr ${prefix}/lib/gnat/deps

clean: force
	${RM} *.gpr 

force:

