SHELL := /bin/bash
#LIBDIR=${HOME}/Rlibs/lib
PACKAGE=mrgsolve
VERSION=$(shell grep Version rdev/DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=rdev/
CHKDIR=Rchecks

## Set libPaths:
export R_LIBS=${LIBDIR}

travis_build:
	make doc
	make build
	make install

readme:
	Rscript -e 'library(rmarkdown); render("README.R")'

all:
	make doc
	make build
	make install


.PHONY: doc
doc:
	Rscript makescripts/doc_mrgsolve.R

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

check-cran:
	make doc
	make build
	R CMD check --as-cran ${TARBALL} -o ${CHKDIR}

test:
	R CMD INSTALL ${PKGDIR}
	Rscript -e 'library(testthat)' -e 'test_dir("rdev/tests/testthat")'

.PHONY: tests
tests:
	Rscript makescripts/tests.R

clean:
	if test -d ${CHKDIR}/mrgsolve.Rcheck; then rm -rf ${CHKDIR}/mrgsolve.Rcheck;fi

datasets:
	Rscript makescripts/datasets.R


