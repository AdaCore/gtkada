
func:
	./generate_doc.pl > generated.texi 2>generated_menu.texi
	tex func.texi
	texindex func.cp
	tex func.texi
	makeinfo func.texi
	texi2html func.texi

clean:
	${RM} func.aux func.cp func.dvi func.fn func.html func.info func.ky 
	${RM} func.log func.pg func.toc func.tp func.vr func_stoc.html func_toc.html 
	${RM} generated.texi generated_menu.texi

