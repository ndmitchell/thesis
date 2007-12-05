mkdir obj

REM Graphics
copy graphics\*.eps obj

REM Tex files
for %%i in (*.tex) do lhs2tex %%i -o obj\%%i

REM Bibtex
copy thesis.bib obj

REM Combine
cd obj
bibtex thesis
texify thesis.tex %1
cd ..

del thesis.dvi
copy obj\thesis.dvi thesis.dvi
