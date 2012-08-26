

%.html: %.texi style.css 
	makeinfo --html --no-split --css-include=style.css $<

%.pdf:  %.texinfo 
	texi2pdf $<

%.info: %.texinfo
	makeinfo --no-split $<

html: e-monotone.html

pdf:  e-monotone.pdf

info: e-monotone.info

pages: e-monotone.html
	git checkout gh-pages
	cp e-monotone.html index.html
	git commit -a -C master
	rm index.html
	git checkout -f master

install-info: e-monotone.html
	cp e-monotone.info ~/share/info
	install-info e-monotone.info ~/share/info/dir
