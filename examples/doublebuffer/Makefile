all: parameters anim anim2

parameters::
	gnatmake -gnatwuwl parameters `gtkada-config`

anim::
	gnatmake -gnatwuwl anim `gtkada-config`

anim2::
	gnatmake -gnatwuwl anim2 `gtkada-config`

clean:
	${RM} *.o *.ali anim parameters anim2

