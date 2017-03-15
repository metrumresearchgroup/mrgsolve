# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
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

project <- file.path(system.file(package="mrgsolve"), "models")

context("Testing NMXML functionality")


code <- '
$NMXML
project=file.path(path.package("mrgsolve"), "nonmem")
run  = 1005
oname="OMEGA", sname="SIGMA"
sigma=TRUE

$PARAM CL=1
$INIT CENT=0
$ODE dxdt_CENT=0;
'
#' 
#' tmp <- tempdir()
#' 
#' test_that("Model spec with $NMXML block can be parsed", {
#'   expect_is(mcode("nmxml1", code,warn=FALSE),"mrgmod")
#' })
#' 
#' mod <- mcode("test6", code)
#' 
#' par <- lapply(as.list(param(mod)), round, digits=3)
#' 
#' test_that("THETAS are imported into the parameter list", {
#'   expect_identical(mrgsolve:::pars(mod), c(paste0("THETA", 1:7),"CL"))
#'   expect_identical(par$THETA6, 1.024)
#'   expect_identical(par$THETA2, 22.791)
#' })
#' 
#' mat <- signif(as.matrix(omat(mod)), 3)
#' test_that("OMEGAS are imported into the omega list", {
#'   expect_equivalent(mat[1,1],0.214)
#'   expect_equivalent(mat[1,2],0.121)
#'   expect_equivalent(mat[3,2],-0.0372)
#' })
#' 
#' mat <- signif(as.matrix(smat(mod)), 3)
#' test_that("SIGMA are imported into the sigma list", {
#'   expect_equivalent(mat[1,1],0.0492)
#'   expect_equivalent(mat[2,2],0.202)
#'   expect_equivalent(mat[1,2],0)
#' })
#' 
#' 
#' code <- '
#' 
#' $OMEGA
#' 1 2 3
#' 
#' $SIGMA
#' 4
#' 
#' $NMXML
#' project=file.path(path.package("mrgsolve"), "nonmem")
#' run  = 1005
#' oname="OMGA", sname="SIGMA"
#' 
#' $OMEGA @corr @name OM
#' 5 0.1 6
#' 
#' $OMEGA @use FALSE
#' 11 12 13
#' 
#' $SIGMA @name sg @block
#' 7 0 8
#' 
#' $PARAM CL=1
#' $INIT CENT=0
#' '
#' mod <- mcode("test6b",code)
#' 
#' context("User OMEGA and SIGMA matrices are loaded correctly")
#' 
#' mat <- signif(as.matrix(omat(mod)),3)
#' 
#' test_that("Loading OMEGA from multiple sources", {
#'   expect_equivalent(dim(mat), c(11,11))
#'   expect_equivalent(mat[2,2],2)
#'   expect_equivalent(mat[4,4],0.214)
#'   expect_equivalent(mat[6,5],-0.0372)
#' })
#' 
#' 
#' test_that("Correlation in corr matrix is converted to covariance", {
#'   expect_equivalent(mat[8,7],0.548)
#' })
#' 
#' test_that("When use=FALSE, variance is 0", {
#'   expect_true(all(mat[9:11,9:11]==0))
#' })
#' 
#' matl <- as.list(omat(mod))
#' mat <- signif(as.list(omat(mod))$OM,3)
#' test_that("Matrices are properly named", {
#'   expect_identical(names(matl), c("...", "OMGA", "OM", "..."))
#'   expect_equivalent(mat[2,1], 0.548)
#' })
#' 
#' mat <- signif(as.matrix(smat(mod)),3)
#' test_that("Loading SIGMA from multiple sources", {
#'   expect_equivalent(dim(mat), c(5,5))
#'   expect_equivalent(mat[2,2], 0.0492)
#'   expect_equivalent(mat[5,5], 8)
#'   expect_equivalent(mat[1,1], 4)
#' })
#' 
#' context("Testing OMEGA and SIGMA updates")
#' a <- bmat(c(1,0.1,3))
#' b <- as.list(omat(update(mod, omega=list(OM=a))))$OM
#' 
#' test_that("Update OMEGA by name", {
#'   expect_identical(a, b)
#' })
#' 
#' a <- dmat(5,5)
#' b <- as.list(smat(mod %>% smat(sg=a)))$sg
#' test_that("Update SIGMA by name", {
#'   expect_identical(a, b)
#' })
#' 
#' 
#' test_that("An error is generated for incompatible dimensions",{
#'   expect_error(mod %>% omat(OM=matrix(1)))
#'   expect_error(mod %>% omat(OM=dmat(1,2,3,4,5,6)))
#'   expect_error(mod %>% smat(sg=dmat(1,2,3)))
#' })
#' 
#' test_that("A warning is generated when nothing is updated",{
#'   expect_warning(mod %>% omat(O=matrix(1)))
#'   expect_warning(mod %>% smat(S=matrix(1)))
#' })
#' 
#' code <- '
#' $INIT a=1
#' 
#' $OMEGA
#' 1 2 3
#' 
#' $SIGMA 
#' 1 2
#' 
#' $SIGMA 
#' 2
#' 
#' '
#' 
#' mod <- mcode("tst_mat_update",code)
#' context("Testing unnamed matrix updates")
#' 
#' a <- dmat(1,2,3)
#' b <- dmat(4,5,6)
#' test_that("A single unnamed matrix is updated", {
#'   expect_equivalent(a, as.matrix(omat(mod)))
#'   expect_equivalent(b, as.matrix(omat(mod %>% omat(b))))
#' })
#' test_that("Warning issued if updating unnamed matrix with named matrix", {
#'   expect_warning(mod %>% omat(a=b))
#' })
#' 
#' context("NMXML with no names specified")
#' 
#' 
#' code <- '
#' $NMXML
#' project=file.path(path.package("mrgsolve"), "nonmem")
#' run  = 1005
#' '
#' mod <- mcode("nmxml512",code,warn=FALSE)
#' 
#' 
#' test_that("No matrices when name not given", {
#'   expect_null(nrow(omat(mod)))
#'   expect_null(nrow(smat(mod)))
#' })
#' 
#' code <- '
#' $NMXML
#' project=file.path(path.package("mrgsolve"), "nonmem")
#' run  = 1005
#' omega=TRUE
#' '
#' mod <- mcode("nmxml2231",code,warn=FALSE)
#' 
#' test_that("Get theta and omega", {
#'   expect_true(nrow(omat(mod))==3)
#'   expect_identical(names(omat(mod)),"...")
#'   expect_null(nrow(smat(mod)))
#' })
#' 
#' 
#' 
#' context("Testing matrix labels")
#' 
#' code <- '
#' $OMEGA 
#' @name OM1
#' @labels a b c d
#' 1 2 3 4
#' 
#' $OMEGA @labels x y z
#' 99 99 99
#' 
#' 
#' $SIGMA @labels e f
#' 5 6 
#' 
#' $SIGMA @labels h i j k l
#' 1 2 3 4 5
#' 
#' '
#' mod <- mcode("label1", code,warn=FALSE)
#' 
#' 
#' test_that("Model compiles", {
#'   expect_is(mod,"mrgmod")
#' })
#' 
#' test_that("Labels are assigned to $OMEGA and $SIGMA", {
#'   expect_equivalent(mod@omega@labels, list(s(a,b,c,d),s(x,y,z)))
#'   expect_equivalent(mod@sigma@labels, list(s(e,f), s(h,i,j,k,l)))
#' })
#'   
#' 
#' 
#' test_that("zero.re actually zeros all matrices", {
#'   x <- mod %>% zero.re %>% omat %>% as.matrix
#'   expect_true(all(as.numeric(x)==0))
#'   x <- mod %>% zero.re %>% smat %>% as.matrix
#'   expect_true(all(as.numeric(x)==0))
#' })
#' 
#' 
#' 
#' code <- '
#' $OMEGA
#' @prefix x_
#' @labels a b c d 
#' 1 2 3 4
#' 
#' $OMEGA
#' 0 0 0
#' 
#' '
#' mod <- mcode("label2", code,warn=FALSE)
#' 
#' test_that("Mixed labels / no labels and prefix", {
#'   expect_equivalent(mod@omega@labels, list(s(x_a,x_b,x_c,x_d),s(.,.,.)))
#' })
#' 
#'   
#'   
#'   
#'   
#'   