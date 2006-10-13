GATE_SRC      = gate.adb
GATE_EX       = gate-in.exe
GATE          = gate
GDIALOG       = gdialog
GTKADA_CONFIG = gtkada-config

GNATMAKELDFLAGS = -largs -L../lib-obj -lgtkada $(GTK_LIBS)

OBJ_DIR=obj
include ../Makefile.common

all: force
	${GNATMAKE} -Ptools ${GPR_BUILD} ${GNATMAKELDFLAGS}

install: force
	-${CHMOD} +x ${GATE} ${GTKADA_CONFIG}
	${INSTALL_SCRIPT} ${GATE} ${bindir}
	${INSTALL_SCRIPT} ${GTKADA_CONFIG} ${bindir}
	${INSTALL_PROGRAM} ${OBJ_DIR}/${GATE_EX} ${bindir}
	${INSTALL_PROGRAM} ${OBJ_DIR}/${GDIALOG} ${bindir}

clean: module_clean

distclean: module_distclean
	-${RM} ${GATE} ${GTKADA_CONFIG}
