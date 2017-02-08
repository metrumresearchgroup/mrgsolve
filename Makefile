SHELL := /bin/bash
#LIBDIR=${HOME}/Rlibs/lib
PACKAGE=mrgsolve
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=Rchecks


## Set libPaths:
##export R_LIBS=${LIBDIR}

pkgdown:
	Rscript -e 'library(pkgdown)' -e 'build_home()' -e 'build_reference(examples=FALSE)' -e 'build_news()'

cran:
	make doc
	make build
	R CMD CHECK --as-cran ${TARBALL} -o ${CHKDIR}

travis_build:
	make doc
	make build
	make install

readme:
	Rscript -e 'library(rmarkdown); render("README.Rmd")'

all:
	make doc
	make build
	make install


.PHONY: doc
doc:
	Rscript inst/maintenance/doc_mrgsolve.R

.PHONY: staticdoc
staticdoc:
	Rscript inst/maintenance/staticdocs.R

build:
	R CMD build --md5 $(PKGDIR)

install:
	R CMD INSTALL --install-tests ${TARBALL}

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD check ${TARBALL} -o ${CHKDIR}

qcheck: 
	make doc
	make build 
	R CMD check ${TARBALL} -o ${CHKDIR} --no-manual --no-codoc


check-cran:
	make doc
	make build
	R CMD check --as-cran ${TARBALL} -o ${CHKDIR}

test:
	R CMD INSTALL ${PKGDIR}
	Rscript -e 'library(testthat)' -e 'test_dir("tests/testthat")'

.PHONY: tests
tests:
	Rscript inst/maintenance/tests.R

clean:
	if test -d ${CHKDIR}/mrgsolve.Rcheck; then rm -rf ${CHKDIR}/mrgsolve.Rcheck;fi

datasets:
	Rscript inst/maintenance/datasets.R

travis:
	make build
	R CMD check --as-cran ${TARBALL} -o ${CHKDIR}
