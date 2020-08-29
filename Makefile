SHELL := /bin/bash
LIBDIR=${HOME}/Rlibs/lib
PACKAGE=mrgsolve
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
export _MRGSOLVE_SKIP_MODLIB_BUILD_=true
LOAD_CANDIDATE=library(mrgsolve, lib.loc="mrgsolve.Rcheck")
TEST_UNIT=testthat::test_dir("inst/maintenance/unit",stop_on_failure=TRUE)

all:
	make doc
	make build
	make install

drone:
	make house
	R CMD build --md5 $(PKGDIR) 
	R CMD check --as-cran ${TARBALL}
	export _MRGSOLVE_SKIP_MODLIB_BUILD_=false
	Rscript -e '$(LOAD_CANDIDATE); $(TEST_UNIT)'
	make spelling

spelling:
	Rscript -e 'spelling::spell_check_package(".")'

covr: 
	Rscript "inst/maintenance/covr.R"

house: 
	Rscript "inst/maintenance/build_housemodel.R"

test-all:
	Rscript inst/maintenance/tests.R

no-test:
	make build
	R CMD check ${TARBALL} --no-tests --no-manual

everything:
	make all
	make pkgdown

pkgdown:
	Rscript "inst/maintenance/pkgdown.R"
	cp -r DOCS/ ../../mrgsolve/docs/
	touch ../../mrgsolve/docs/.nojekyll

unit:
	Rscript -e 'testthat::test_dir("inst/maintenance/unit")'

cran:
	make house
	make doc
	make build
	export _MRGSOLVE_SKIP_MODLIB_BUILD_=false
	R CMD CHECK --as-cran ${TARBALL}

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

.PHONY: doc
doc:
	Rscript -e "roxygen2::roxygenize()"

build:
	R CMD build --md5 $(PKGDIR) --no-manual

install:
	R CMD INSTALL --install-tests ${TARBALL} -l ~/Rlibs

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make house
	make doc
	make build
	R CMD check ${TARBALL} --no-manual
	make unit

qcheck: 
	make doc
	make build 
	R CMD check ${TARBALL} --no-manual --no-codoc

check-cran:
	make house
	make doc
	make build
	R CMD check --as-cran ${TARBALL}

test:
	R CMD INSTALL ${PKGDIR}
	make test-all

test1:
	Rscript -e 'testthat::test_file("tests/testthat.R")'

test2:
	Rscript -e 'testthat::test_dir("inst/maintenance/unit")'

clean:
	rm src/*.o
	rm src/*.so

datasets:
	Rscript inst/maintenance/datasets.R

rhub:
	Rscript -e 'rhub::check_for_cran(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false"))'
	
check-fedora:
	Rscript -e 'rhub::check_on_fedora(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false"))'

check-devel: 
	Rscript -e 'rhub::check_with_rdevel()'

check-win:
	Rscript -e 'devtools::check_win_devel()'

check-winhub:
	Rscript -e 'rhub::check_on_windows()'

.PHONY: doxygen
doxygen: 
	doxygen doxyfile

bump-dev:
	Rscript -e 'usethis::use_version("dev")'

tag-version:
	git tag $(VERSION)
	git push origin $(VERSION)

testing:
	cp ${TARBALL} ${MRGSOLVE_TEST_LOC}
	touch ${MRGSOLVE_TEST_LOC}/${TARBALL}
	cp -r inst/maintenance/unit ${MRGSOLVE_TEST_LOC}
	cd ${MRGSOLVE_TEST_LOC} && git commit -am "testing release" && git push -u origin master
