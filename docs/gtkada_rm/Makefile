
gtkada_rm:
	./generate_doc.pl ../../src/*.ads ../../src/pixbuf/*.ads
	echo x | tex gtkada_rm.texi
	texindex gtkada_rm.cp
	tex gtkada_rm.texi
	dvips gtkada_rm.dvi -o gtkada_rm.ps
	perl ../texi2html -menu -split_chapter -body "BGCOLOR=#FFFFFF" gtkada_rm.texi
	makeinfo --force -o gtkada_rm.info gtkada_rm.texi
	makeinfo --force --no-headers --no-split -o gtkada_rm.txt gtkada_rm.texi
	-${RM} *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr
	-${RM} generated.texi generated_menu.texi

clean:
	${RM} *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr
	${RM} *.ps *.html *.info* *.txt
	${RM} generated.texi generated_menu.texi

