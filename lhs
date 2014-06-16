#!/usr/bin/bash

lhs2tex --tt $1.lhs > $1.tex
pdflatex $1.tex
C:/Users/draumart/AppData/Local/Apps/Evince-2.32.0.145/bin/evince.exe $1.pdf &


