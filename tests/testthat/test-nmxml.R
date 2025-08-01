# Copyright (C) 2013 - 2020  Metrum Research Group
#
# This file is part of mrgsolve.
#
# mrgsolve is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsolve is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.

library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-nmxml")

if(!requireNamespace("xml2",quietly=TRUE)) skip("xml2 is not installed.")


code <- '
$NMXML
project = file.path(path.package("mrgsolve"), "nonmem")
run  = 1005
oname="OMEGA", sname="SIGMA"
sigma=TRUE

$PARAM CL=1
$INIT CENT=0
$ODE dxdt_CENT=0;
'

tmp <- tempdir()

test_that("Model spec with $NMXML block can be parsed", {
  expect_is(mcode("nmxml1", code, warn=FALSE, compile = FALSE),"mrgmod")
})

mod <- mcode("test6", code, compile = FALSE)

par <- lapply(as.list(param(mod)), round, digits=3)

test_that("THETAS are imported into the parameter list", {
  expect_identical(mrgsolve:::pars(mod), c(paste0("THETA", 1:7),"CL"))
  expect_identical(par$THETA6, 1.024)
  expect_identical(par$THETA2, 22.791)
})

mat <- signif(as.matrix(omat(mod)), 3)
test_that("OMEGAS are imported into the omega list", {
  expect_equivalent(mat[1,1],0.214)
  expect_equivalent(mat[1,2],0.121)
  expect_equivalent(mat[3,2],-0.0372)
})

mat <- signif(as.matrix(smat(mod)), 3)
test_that("SIGMA are imported into the sigma list", {
  expect_equivalent(mat[1,1],0.0492)
  expect_equivalent(mat[2,2],0.202)
  expect_equivalent(mat[1,2],0)
})

code <- '

$OMEGA
1 2 3

$SIGMA
4

$NMXML
project=file.path(path.package("mrgsolve"), "nonmem")
run  = 1005
oname="OMGA", sname="SIGMA"

$OMEGA @corr @name OM
5 0.1 6

$OMEGA @use FALSE
11 12 13

$SIGMA @name sg @block
7 0 8

$PARAM CL=1
$INIT CENT=0
'
mod <- mcode("test6b",code, compile = FALSE)

mat <- signif(as.matrix(omat(mod)),3)

test_that("Loading OMEGA from multiple sources", {
  expect_equivalent(dim(mat), c(11,11))
  expect_equivalent(mat[2,2],2)
  expect_equivalent(mat[4,4],0.214)
  expect_equivalent(mat[6,5],-0.0372)
})

test_that("Correlation in corr matrix is converted to covariance", {
  expect_equivalent(mat[8,7],0.548)
})

test_that("When use=FALSE, variance is 0", {
  expect_true(all(mat[9:11,9:11]==0))
})

matl <- as.list(omat(mod))
mat <- signif(as.list(omat(mod))$OM,3)
test_that("Matrices are properly named", {
  expect_identical(names(matl), c("...", "OMGA", "OM", "..."))
  expect_equivalent(mat[2,1], 0.548)
})

mat <- signif(as.matrix(smat(mod)),3)
test_that("Loading SIGMA from multiple sources", {
  expect_equivalent(dim(mat), c(5,5))
  expect_equivalent(mat[2,2], 0.0492)
  expect_equivalent(mat[5,5], 8)
  expect_equivalent(mat[1,1], 4)
})


a <- bmat(c(1,0.1,3))
b <- as.list(omat(update(mod, omega=list(OM=a))))$OM

test_that("update OMEGA by name", {
  expect_identical(a, b)
})

a <- dmat(5,5)
b <- as.list(smat(mod %>% smat(sg=a)))$sg
test_that("Update SIGMA by name", {
  expect_identical(a, b)
})

test_that("error is generated for incompatible dimensions",{
  expect_error(mod %>% omat(OM=matrix(1)))
  expect_error(mod %>% omat(OM=dmat(1,2,3,4,5,6)))
  expect_error(mod %>% smat(sg=dmat(1,2,3)))
})

test_that("A warning is generated when nothing is updated",{
  expect_warning(mod %>% omat(O=matrix(1)))
  expect_warning(mod %>% smat(S=matrix(1)))
})

code <- '
$INIT a=1

$OMEGA
1 2 3

$SIGMA 
1 2

$SIGMA 
2

'

mod <- mcode("tst_mat_update",code, compile = FALSE)

a <- dmat(1,2,3)
b <- dmat(4,5,6)
test_that("A single unnamed matrix is updated", {
  expect_equivalent(a, as.matrix(omat(mod)))
  expect_equivalent(b, as.matrix(omat(mod %>% omat(b))))
})
test_that("Warning issued if updating unnamed matrix with named matrix", {
  expect_warning(mod %>% omat(a=b))
})

code <- '
$NMXML
project=file.path(path.package("mrgsolve"), "nonmem")
run  = 1005
omega = FALSE, sigma = FALSE
'
mod <- mcode("nmxml512", code, warn=FALSE, compile = FALSE)

test_that("No matrices when name not given", {
  expect_null(nrow(omat(mod)))
  expect_null(nrow(smat(mod)))
})

code <- '
$NMXML
project=file.path(path.package("mrgsolve"), "nonmem")
run  = 1005
sigma = FALSE
'
mod <- mcode("nmxml2231",code,warn=FALSE, compile = FALSE)

test_that("Get theta and omega", {
  expect_true(nrow(omat(mod))==3)
  expect_identical(names(omat(mod)),"...")
  expect_null(nrow(smat(mod)))
})

code <- '
$OMEGA 
@name OM1
@labels a b c d
1 2 3 4

$OMEGA @labels x y z
99 99 99

$SIGMA @labels e f
5 6 

$SIGMA @labels h i j k l
1 2 3 4 5

'
mod <- mcode("label1", code, warn=FALSE, compile = FALSE)

test_that("Model compiles", {
  expect_is(mod,"mrgmod")
})

test_that("Labels are assigned to $OMEGA and $SIGMA", {
  expect_equivalent(mod@omega@labels, list(s_(a,b,c,d),s_(x,y,z)))
  expect_equivalent(mod@sigma@labels, list(s_(e,f), s_(h,i,j,k,l)))
})

test_that("zero_re zeros all matrices", {
  x <- mod %>% zero_re %>% omat %>% as.matrix
  expect_true(all(as.numeric(x)==0))
  x <- mod %>% zero_re %>% smat %>% as.matrix
  expect_true(all(as.numeric(x)==0))
})

code <- '
$OMEGA
@prefix x_
@labels a b 
1 2 

$OMEGA
0 0 0

'
mod <- mcode("label2", code,warn=FALSE, compile = FALSE)

test_that("Mixed labels / no labels and prefix", {
  expect_equivalent(mod@omega@labels, list(s_(x_a,x_b),s_(.,.,.)))
})

test_that("read_nmext returns estimates", {
  project <- system.file("nonmem", package="mrgsolve")
  x <- read_nmext(1005, project)   
  expect_equal(names(x), c("raw", "param", "omega", "sigma"))
  expect_is(x$param, "list")
  expect_is(x$omega, "matrix")
  expect_is(x$sigma, "matrix")
  
  x2 <- read_nmext(path=file.path(project, 1005, "1005.ext"))
  expect_identical(x,x2)
})

test_that("NONMEM estimates from nmext", {
  project <- system.file("nonmem", package="mrgsolve")
  a <- mrgsolve:::nmext(run = 1005, project = project)
  expect_is(a$theta, "list")
  expect_is(a$omega, "omegalist")
  expect_is(a$sigma, "sigmalist")
  expect_error(mrgsolve:::nmext(run = 10051, project = project), 
               "could not find the requested")
  expect_error(mrgsolve:::nmext(run = 1005, project = project,read_fun = "foo"),
               "'arg' should be one of ")
})

test_that("NONMEM estimates from nmext - multiple tables", {
  project <- system.file("nonmem", package="mrgsolve")
  
  a <- mrgsolve:::nmext(run = 2005, project = project, index = "last")
  a_att <- attributes(read_nmext(run = 2005, project = project, index = "last"))
  expect_equal(a_att$index,5)
  expect_match(a_att$table, "First Order Conditional")
  
  b <- mrgsolve:::nmext(run = 2005, project = project, index = 2)
  b_att <- attributes(read_nmext(run = 2005, project = project, index = 2))
  expect_equal(b_att$index,2)
  expect_match(b_att$table, "Importance Sampling")
  
  expect_true(a$theta$THETA3 != b$theta$THETA3)
  
  rtab <- read_nmext(
    run = 2005, project = project, index = 3,  
    read_fun = "read.table"
  )
  expect_is(rtab, "list")
  rtab_att <- attributes(rtab)
  expect_match(rtab_att$table, "Stochastic Approximation")
  
  d <- read_nmext(run = 1005, project = project, index = "single")
  e <- read_nmext(run = 1005, project = project, index = 1)
  d_attr <- attributes(d)
  expect_match(d_attr$table, "single")
  expect_equivalent(d,e)
  expect_identical(d$raw, e$raw)
  
  expect_error(
    mrgsolve:::nmext(run = 2005, project = project, index = 333), 
    regexpr = "table 333 requested but only 5 tables"
  )
})

test_that("custom labeled THETA", {
  project <- system.file("nonmem", package = "mrgsolve")
  a <- mrgsolve:::nmxml(run = 1005, project = project)
  b <- mrgsolve:::nmxml(run = 1005, project = project, tname = letters[1:7])
  expect_identical(names(a$theta), paste0("THETA", 1:7))
  expect_identical(names(b$theta), letters[1:7])
  expect_error(mrgsolve:::nmxml(run=1005,project=project,tname=letters[1:6]))
})

test_that("read nm estimates relative to cpp file", {
  skip_if_not(
    all(
      file.exists("nm/1005-ext.cpp"), 
      file.exists("nm/1005-xml.cpp")
    )
  )
  mod <- mread("1005-ext", project = "nm", compile = FALSE)
  expect_is(mod, "mrgmod") 
  mod <- mread("1005-xml", project = "nm", compile = FALSE)
  expect_is(mod, "mrgmod") 
})

test_that("nm source file is available via as.list", {
  skip_if_not(
    all(
      file.exists("nm/1005-ext.cpp"), 
      file.exists("nm/1005-xml.cpp"), 
      file.exists("nm/1005-both.cpp")
    )
  )
  list1 <- as.list(mread("1005-ext", project = "nm", compile = FALSE))
  list2 <- as.list(mread("1005-xml", project = "nm", compile = FALSE))
  list3 <- as.list(mread("1005-both", project = "nm", compile = FALSE))
  ans <- c("1005.ext", "1005.xml")
  expect_equal(basename(list1[["nm_import"]]), ans[1])
  expect_equal(basename(list2[["nm_import"]]), ans[2])
  expect_equal(basename(list3[["nm_import"]]), ans)
})

test_that("use cpp file stem as nm run number nmext [SLV-TEST-0021]", {
  skip_if_not(file.exists("nm/cppstem-nmext/1005.cpp"))
  mod <- mread("1005", project = "nm/cppstem-nmext", compile = FALSE)
  expect_is(mod, "mrgmod")
  nmext_file <- basename(as.list(mod)[["nm_import"]])
  expect_equal(nmext_file, "1005.ext")
})

test_that("use cpp file stem as nm run number nmxml [SLV-TEST-0022]", {
  skip_if_not(file.exists("nm/cppstem-nmxml/1005.cpp"))
  mod <- mread("1005", project = "nm/cppstem-nmxml", compile = FALSE)
  expect_is(mod, "mrgmod")
  nmxml_file <- basename(as.list(mod)[["nm_import"]])
  expect_equal(nmxml_file, "1005.xml")
})

test_that("provide path rather than run and project [SLV-TEST-023]", {
  skip_if_not(file.exists("nm/1005-path-ext.mod"))
  mod <- mread("1005-path-ext.mod", project = "nm", compile = FALSE)
  expect_is(mod, "mrgmod")
  nmext_file <- basename(as.list(mod)[["nm_import"]])
  expect_equal(nmext_file, "1005.ext")
  
  skip_if_not(file.exists("nm/1005-path-xml.mod"))
  mod <- mread("1005-path-xml.mod", project = "nm", compile = FALSE)
  expect_is(mod, "mrgmod")
  nmxml_file <- basename(as.list(mod)[["nm_import"]])
  expect_equal(nmxml_file, "1005.xml")
})

test_that("return all non-skipped matrices gh-1274", {
  skip_if_not(file.exists("nm/1005-omega-skip.mod"))
  mod <- mread("1005-omega-skip.mod", project = "nm", compile = FALSE) 
  expect_is(mod, "mrgmod")
  expect_equivalent(length(omat(mod)), 1)
  expect_equivalent(nrow(omat(mod)), 3)
})
