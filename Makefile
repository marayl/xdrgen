.PHONY: all
all: Test.pdf

.PHONY: clean
clean:
	rm -f Test.aux Test.dvi Test.log Test.nlo Test.out Test.pdf Test.ptb Test.tex Test.toc

Test.pdf: Test.dvi
	dvipdfm $<

Test.dvi: Test.tex
	latex $<

Test.tex: Test.lhs
	lhs2TeX $< >$@
