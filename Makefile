SHELL := /bin/bash
PACKAGE=mrgsolve
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
export _MRGSOLVE_SKIP_MODLIB_BUILD_=yes
LOAD_CANDIDATE=library(mrgsolve, lib.loc="mrgsolve.Rcheck")
TEST_UNIT=testthat::test_dir("inst/maintenance/unit",stop_on_failure=TRUE)

all:
	make doc
	make build
	make install

bump-dev:
	Rscript -e 'usethis::use_version("dev")'

tag-version:
	git tag $(VERSION)
	git push origin $(VERSION)

package:
	make house
	make doc
	make build
	make install
	make pkgdown

check:
	make house
	make doc
	make build
	R CMD check ${TARBALL} --no-manual
	make unit

check-only:
	make doc
	R CMD check  ${TARBALL} --no-manual --no-test

cran: export _MRGSOLVE_SKIP_MODLIB_BUILD_=no
cran:
	make house
	make doc
	make build
	R CMD CHECK --as-cran ${TARBALL}

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

pkgdown:
	Rscript "inst/maintenance/pkgdown.R"
	#cp -r DOCS/ ../mrgsolve/docs/
	#touch ../mrgsolve/docs/.nojekyll

unit:
	Rscript -e 'testthat::test_dir("inst/maintenance/unit")'

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
	if [ -d mrgsolve.Rcheck ]; then rm -Rf mrgsolve.Rcheck; fi
	R CMD REMOVE mrgsolve

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
	
modlib: export _MRGSOLVE_SKIP_MODLIB_BUILD_=no
modlib: 
	Rscript -e 'testthat::test_file("inst/maintenance/unit/test-modlib.R")'

# possibly no longer in use
drone:
	make house
	R CMD build --md5 $(PKGDIR) 
	R CMD check --as-cran ${TARBALL}
	export _MRGSOLVE_SKIP_MODLIB_BUILD_=false
	Rscript -e '$(LOAD_CANDIDATE); $(TEST_UNIT)'
