include ../src/Makefile.common

examples=$(prefix)/examples/gtkada/

DIRECTORIES=base_widget \
  calendar_combo        \
  composite_widget      \
  doublebuffer          \
  documentation         \
  dynamic_cast          \
  editor                \
  gtk_examples          \
  tutorial              \
  handlers		\
  powergnu		\
  user_data

TUTORIAL_DIRS=tutorial/gtkdial \
  tutorial/helloworld  \
  tutorial/helloworld2 \
  tutorial/packbox     \
  tutorial/simple      \
  tutorial/table       \
  tutorial/tictactoe

SHELL=sh

all: ${DIRECTORIES}
	@for file in ${DIRECTORIES}; do \
	  make -C $${file} all; \
	done

install:
	mkdir -p $(examples)
	-for dir in ${DIRECTORIES} ${TUTORIAL_DIRS}; do \
	  mkdir -p $(examples)/$${dir}; \
	  rm -f $${dir}/b~*; \
	  cp -p $${dir}/*.ad? $(examples)/$${dir} 2>/dev/null; \
	  cp -p $${dir}/Makefile $(examples)/$${dir} 2>/dev/null; \
	  cp -p $${dir}/README $(examples)/$${dir} 2>/dev/null; \
	done

clean::
	@for file in ${DIRECTORIES}; do \
	  make -C $${file} clean; \
	done

