
MKDIR = mkdir -p

gtkada_rm: docgen docgen_clean

docgen:
	$(MKDIR) gtkada_rm
	$(MKDIR) doc_obj
	gnatdoc -Pgtkada_documentation.gpr

docgen_clean:
	$(RM) -rf doc_obj/*

clean: docgen_clean
	$(RM) generated.texi generated_menu.texi
	$(RM) -rf gtkada_rm/

distclean: clean

force:
