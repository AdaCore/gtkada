
func:
	./generate_doc.pl ../../src/*.ads
	tex func.texi
	texindex func.cp
	tex func.texi
	makeinfo func.texi
	perl ../texi2html func.texi

clean:
	${RM} func.aux func.cp func.dvi func.fn func.html func.info* func.ky 
	${RM} func.log func.pg func.toc func.tp func.vr func_stoc.html func_toc.html 
	${RM} generated.texi generated_menu.texi func.cps generated_submenu.texi

