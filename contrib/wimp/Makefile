OBJS = wimp_rc_style.o wimp_style.o wimp_theme_main.o

CFLAGS = -O2

all: $(OBJS) libwimp.dll

.c.o:
	$(CC) -c $(CFLAGS) -I. `pkg-config gtk+-2.0 --cflags` $<

libwimp.dll: $(OBJS)
	$(CC) -shared -o libwimp.dll $(OBJS) `pkg-config gtk+-2.0 --libs`

clean:
	$(RM) $(OBJS) libwimp.dll
