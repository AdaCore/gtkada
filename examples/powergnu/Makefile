CFLAGS = -g -O2

EXEC = power

all: $(EXEC)

power: force
	gnatmake -m -gnatwu $(CFLAGS) $(EXEC) `gtkada-config`

force:

clean:
	rm -f *.o *.ali *~ b~* $(EXEC)
