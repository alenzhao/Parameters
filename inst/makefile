default: quickbuild install

help:
	egrep "^#" makefile | sed "s/^#//"

# --- quickbuild: no vignette
#
quickbuild:
	(cd ../..; R CMD build --no-vignettes Parameters)


# --- build
#
build:
	(cd ../..; R CMD build Parameters)

# --- install
#
install: quickbuild
	(cd ../..; R CMD install Parameters)

# --- check
#
check: build install
	(cd ../..; R CMD check --no-manual --no-vignettes --no-codoc --no-examples --no-manual  Parameters)

# --- checkfull
#
checkfull: build install
	(cd ../..; R CMD check Parameters)


# --- vanillaTest
# run all the unit tests, in a clean context
#

vanillaTest:  build install
	- rm vanillaTest.out
	R --vanilla < vanillaTest.R > vanillaTest.out 2>&1

# --- vt
# run all the unit tests, in a clean context
#

vt: vanillaTest



# --- checkvig
# check just the vignette
#

checkvig:
	(cd ../..; R CMD check --no-manual --no-codoc --no-tests --no-examples Parameters)


# --- tangle
# extract the R code from the vignette file
#

tangle:
	(cd doc; R CMD Stangle $(PACKAGE_NAME).Rnw)



# --- sweave
# creates Parameters.tex, runs all embedded examples
# run this before the pdf target
#
sweave: 
	(cd doc; R CMD Sweave $(PACKAGE_NAME).Rnw)

# --- pdf
# make and open Parameters.pdf, the vignette 
#

pdf:  sweave
	(cd doc; rm *.pdf; /usr/texbin/latex --output-format=pdf  Parameters.tex; open Parameters.pdf)



