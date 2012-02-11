################################################################################
# Copyright 2011
# Andrew Redd
# 11/23/2011
#
# Description of File:
# Makefile for knitr compiling
# 
################################################################################
all:pdf  # default rule DO NOT EDIT
################################################################################
MAINFILE  := fANOVA
RNWFILES  := 
RFILES    := 
TEXFILES  := 
CACHEDIR  := cache
FIGUREDIR := figures
LATEXMK_FLAGS := 
##### Explicit Dependencies #####
################################################################################
RNWTEX = $(RNWFILES:.Rnw=.tex)
ROUTFILES = $(RFILES:.R=.Rout)
RDATAFILES= $(RFILES:.R=.Rdata)
MAINTEX = $(MAINFILE:=.tex)
MAINPDF = $(MAINFILE:=.pdf)
ALLTEX = $(MAINTEX) $(RNWTEX) $(TEXFILES)
KNITCMD="library('knitr'); \
knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-');\
knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-');\
knit('$<','$@')"

# Dependencies
$(RNWTEX): $(RDATAFILES)
$(MAINTEX): $(RNWTEX) $(TEXFILES)
$(MAINPDF): $(MAINTEX) $(ALLTEX) 

.PHONY:pdf tex clean clearcache cleanall
pdf: $(MAINPDF)
tex: $(RDATAFILES) $(ALLTEX) 

$(CACHEDIR):
	mkdir $(CACHEDIR)
	
$(FIGUREDIR):
	mkdir $(FIGUREDIR)

%.tex:%.Rnw | $(CACHEDIR) $(FIGUREDIR)
	Rscript -e $(KNITCMD)

%.R:%.Rnw
	Rscript -e "Sweave('$^', driver=Rtangle())"

%.Rout:%.R
	R CMD BATCH "$^" "$@"

%.pdf: %.tex 
	latexmk -pdf $<
	
clean:
	-latexmk -c -quiet $(MAINFILE).tex
	-rm -f $(MAINTEX) $(RNWTEX)
	-rm -rf $(FIGUREDIR)
	-rm *tikzDictionary
	-rm $(MAINPDF)
	
clearcache:
	-rm -rf cache
  
cleanall: clean clearcache
