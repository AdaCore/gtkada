DIRECTORIES=base_widget \
  composite_widget      \
  doublebuffer          \
  documentation         \
  dynamic_cast          \
  editor                \
  gtk_examples          \
  tutorial              \
  handlers

SHELL=sh

all: ${DIRECTORIES}
	@for file in ${DIRECTORIES}; do \
	  make -C $${file} all; \
	done

clean::
	@for file in ${DIRECTORIES}; do \
	  make -C $${file} clean; \
	done
