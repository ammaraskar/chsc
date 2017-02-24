all:
	cabal build

bloom.out:
	./test-bloom > bloom.out 2>&1

bloom.tex: bloom.out
	perl out-to-tex.pl bloom.out > bloom.tex

bloom.pdf: bloom.tex
	pdflatex bloom.tex
