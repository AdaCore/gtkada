
gtkada_rm:
	./generate_doc.pl ../../src/*.ads
	tex gtkada_rm.texi
	texindex gtkada_rm.cp
	tex gtkada_rm.texi
	makeinfo gtkada_rm.texi
	perl ../texi2html gtkada_rm.texi

clean:
	${RM} gtkada_rm.aux gtkada_rm.cp gtkada_rm.dvi gtkada_rm.fn gtkada_rm.html 
	${RM} gtkada_rm.info* gtkada_rm.ky gtkada_rm.log gtkada_rm.pg gtkada_rm.toc
	${RM} gtkada_rm.tp gtkada_rm.vr gtkada_rm_stoc.html gtkada_rm_toc.html 
	${RM} generated.texi generated_menu.texi gtkada_rm.cps generated_submenu.texi

