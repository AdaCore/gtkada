all: gtkada.texi
	tex gtkada.texi
	texindex gtkada.cp
	tex gtkada.texi
	dvips gtkada.dvi -o gtkada.ps
	perl ./texi2html -menu -monolithic gtkada.texi
	makeinfo --force -o gtkada.info gtkada.texi
	makeinfo --force --no-headers --no-split -o gtkada.txt gtkada.texi
	rm -f *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr
