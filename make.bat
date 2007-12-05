mkdir obj
copy *.* obj\*.*
chdir obj
lhs2tex thesis.tex -o final.tex
bibtex thesis
texify final.tex %1
cd ..
del thesis.dvi
copy obj\final.dvi thesis.dvi
