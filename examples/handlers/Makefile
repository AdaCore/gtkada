all: delete_event specific_tips.o general_tips.o

delete_event:
	gnatmake delete_event `gtkada-config`

specific_tips.o:
	gnatmake -c specific_tips `gtkada-config`

general_tips.o:
	gnatmake -c general_tips `gtkada-config`

clean:
	${RM} *.o *.ali b~* delete_event

