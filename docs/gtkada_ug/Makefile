all: gtkada_ug.texi
	echo x | tex gtkada_ug.texi
	texindex gtkada_ug.cp
	tex gtkada_ug.texi
	dvips gtkada_ug.dvi -o gtkada_ug.ps
	perl ../texi2html -menu -monolithic gtkada_ug.texi
	makeinfo --force -o gtkada_ug.info gtkada_ug.texi
	makeinfo --force --no-headers --no-split -o gtkada_ug.txt gtkada_ug.texi
	-$(RM) *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr

clean:
	-$(RM) *.aux *.cp *.cps *.dvi *.fn *.ky *.log *.pg *.toc *.tp *.vr
	-$(RM) gtkada_ug.html gtkada_ug.info* gtkada_ug.ps gtkada_ug.txt
