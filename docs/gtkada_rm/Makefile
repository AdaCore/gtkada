
SRC=../../src/*.ads ../../src/opengl/*.ads ../../src/pixbuf/*.ads

gtkada_rm: ps html info partial_clean

partial_clean::
	-${RM} *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr
	-${RM} generated.texi generated_menu.texi

generated.texi : ${SRC} generate_doc.pl
	./generate_doc.pl ${SRC}

ps: gtkada_rm.ps
gtkada_rm.ps: generated.texi
	echo x | tex gtkada_rm.texi
	texindex gtkada_rm.cp
	tex gtkada_rm.texi
	dvips gtkada_rm.dvi -o gtkada_rm.ps
	
html: gtkada_rm_toc.html
gtkada_rm_toc.html: generated.texi
	perl ../texi2html -menu -split_chapter -body "BGCOLOR=#FFFFFF" gtkada_rm.texi

info: gtkada_rm.info
gtkada_rm.info: generated.texi
	makeinfo --force -o gtkada_rm.info gtkada_rm.texi
	makeinfo --force --no-headers --no-split -o gtkada_rm.txt gtkada_rm.texi

clean:
	${RM} *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr
	${RM} *.ps *.html *.info* *.txt
	${RM} generated.texi generated_menu.texi

