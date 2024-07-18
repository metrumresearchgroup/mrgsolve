# Copyright (C) 2013 - 2021  Metrum Research Group
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

context("test-modspec")

options(mrgsolve_mread_quiet=TRUE)

new_test_build <- function(model = "pk1", project = tempdir()) {
  file.copy(file.path(modlib(), paste0(model, ".cpp")), project, overwrite = TRUE)
  mrgsolve:::new_build(model = model, project = project)
}

mtemp <- function(...) {
  mcode(model=basename(tempfile()),..., compile=FALSE)
}

test_that("matrix data is parsed", {
  
  code <- "$OMEGA \n 1 2 \n 3"
  mod <- mtemp(code)
  expect_equal(dim(omat(mod))[[1]],c(3,3))
  
  code <- "$OMEGA \n @block \n 1 0.002 \n 3"
  mod <- mtemp(code)
  expect_equal(dim(omat(mod))[[1]],c(2,2))  
})

test_that("capture data is parsed", {
  code <- "$CAPTURE\n  \n banana = b z apple = a"
  mod <- mtemp(code)
  expect_equal(mod@capture, c(b = "banana", z = "z", a = "apple"))
  
  code <- "$CAPTURE\n  z a \n\n\n d\n e, f"
  mod <- mtemp(code)
  expect_equal(
    mod@capture, 
    c(z = "z", a = "a", d = "d", e = "e", f = "f")
  )
  code <- "$CAPTURE \n"
  expect_warning(mod <- mtemp(code))
  expect_equivalent(mod@capture, character(0))
})


test_that("cmt block is parsed", {
  code <- "$CMT\n yes=TRUE \n first \n \n \n second third \n \n"
  mod <- mtemp(code)
  expect_equal(mrgsolve:::cmt(mod), c("first", "second", "third"))
})

test_that("theta block is parsed", {
  code <- "$THETA\n  0.1 0.2 \n 0.3"
  mod <- mtemp(code)
  expect_equal(param(mod), param(THETA1=0.1, THETA2=0.2, THETA3=0.3))
  
  code <- "$THETA\n name='theta' \n  0.1 0.2 \n 0.3"
  mod <- mtemp(code)
  expect_equal(param(mod), param(theta1=0.1, theta2=0.2, theta3=0.3))
  
  code <- "$THETA >> name='theta' \n  0.1 0.2 \n 0.3"
  mod <- mtemp(code)
  expect_equal(param(mod), param(theta1=0.1, theta2=0.2, theta3=0.3))
})

test_that("Using table macro generates error", {
  code <- "$TABLE\n table(CP) = 1; \n double x=3; \n table(Y) = 1;"
  expect_error(mod <- mtemp(code))
})

for(what in c("THETA", "PARAM", "CMT", 
              "FIXED", "CAPTURE", "INIT",
              "OMEGA", "SIGMA")) {
  
  test_that(paste0("Empty block: ", what), {
    expect_warning(mtemp(paste0("$",what, "  ")))
  })
}

test_that("Commented model", {
  code <- '
  // A comment
  $PARAM CL = 2## comment
  VC = 10
  
  KA=3
  $INIT x=0, y = 3 // Hey
  ## comment
  h = 3 ## yo
  ## comment
  $TABLE
  capture a=2;//
  double b = 3;
  ## 234234
  $CAPTURE 
    kaya = KA // Capturing KA
  ' 
  
  expect_is(mod <- mcode("commented", code,compile=FALSE),"mrgmod")
  expect_identical(param(mod),param(CL=2,VC=10,KA=3))
  expect_identical(init(mod),init(x=0,y=3,h=3))
  expect_identical(mod@capture, c(KA = "kaya",a = "a"))
})


test_that("at options are parsed", {
  
  ats <- mrgsolve:::parse_ats
  
  code <- '
  
  @bool1
  @bool2

  @name some person
  @  zip   55455 @town minneapolis @city
  @ state mn @midwest @x 2
  
  @!yellow
  '
  
  x <- unlist(strsplit(code, "\n"))
  x <- ats(x)
  expect_equal(
    names(x), 
    c("bool1", "bool2", "name", "zip", "town", "city", "state", "midwest", "x", 
      "yellow")
  )
  expect_is(x,"list")
  expect_identical(x$bool1,TRUE)
  expect_identical(x$bool2,TRUE)
  expect_identical(x$city,TRUE)
  expect_identical(x$midwest,TRUE)
  expect_identical(x$name,"some person")
  expect_identical(x$state,"mn")  
  expect_identical(x$town,"minneapolis")
  expect_equal(x$x,2)
  expect_equal(x$yellow, FALSE)
  expect_warning(ats(" @hrm ' a b c'"))  
  expect_warning(ats('@foo "a b c"'))  
})

test_that("HANDLEMATRIX", {
  code <- "$OMEGA 1,2,3"
  mod <- mcode("test-spec-matrix", code, compile = FALSE)
  mat <- unname(as.matrix(omat(mod)))
  expect_true(all.equal(mat, dmat(1,2,3)))
})

test_that("inventory of internal variables", {
  code <- '
[ global ] 
#define a 1
int b = 2; 

[ main ] 
double c = 3;

[ ode ] 
double d = 4;
dxdt_f = 0;

[ table ] 
bool e = true;

[ cmt ] f; 
'
  mod <- mcode("test-variables", code, compile = FALSE)
  ans <- as.list(mod)$cpp_variables
  expect_is(ans, "data.frame")
  expect_equal(names(ans), c("type", "var", "context"))
  expect_equal(ans$var, letters[1:5])
  expect_equal(
    ans$type, 
    c("define", "int", "double", "double", "bool")
  )
  expect_equal(
    ans$context, 
    c("global", "global", "main", "ode", "table")
  )
})

test_that("programmatic initialization", {
  code <- '
$ENV
mat1 <- matrix(0,1,1)
mat2 <- matrix(0,2,2)
mat3 <- matrix(0,3,3)
rownames(mat3) <- letters[1:3]
mat4 <- matrix(0,4,4)
par <- list(z = 777)
pcmt <- c("t", "u", "v")

$OMEGA
1

$OMEGA @object mat2 @name omega2

$OMEGA @as_object 
m <- matrix(0,3,3)
rownames(m) <- LETTERS[1:3]
m

$SIGMA
11

$SIGMA @object mat4
$SIGMA @as_object
matrix(0,5,5)

$PARAM @as_object
list(a = 1, b = 2)

$THETA @as_object @name theta
rep(0,2)

$PARAM @object par

$CMT @as_object
c("gg", "hh", "iii")

$CMT @object pcmt
'
  
  mod <- mcode("foo", code, compile = FALSE)
  x <- labels(mod)
  expect_equal(x$param, c("a", "b", "theta1", "theta2", "z"))
  expect_equal(mod$z, 777)
  expect_equal(x$omega_labels[[3]], c("A", "B", "C"))
  expect_equal(x$omega[2], "omega2")
  d <- nrow(omat(mod))
  expect_equal(unname(nrow(omat(mod))), c(1,2,3))
  expect_equal(unname(nrow(smat(mod))), c(1,4,5))
  expect_equal(x$init, c("gg", "hh", "iii", "t", "u", "v"))
})

test_that("parse content using low-level handlers - PARAM", {
  build <- new_test_build()
  env <- mrgsolve:::parse_env(vector(mode = "list", length = 20), build = build)
  sup <- suppressMessages  
  
  input <- "c(1,2,3)"
  expect_error(
    sup(mrgsolve:::PARAM(x = input, as_object = TRUE)), 
    "the returned object was the wrong type"
  )
  input <- "list(a = 1, b = 2)"
  ans <- mrgsolve:::PARAM(x = input, as_object = TRUE, env = env, pos = 3)
  expect_is(env$param[[3]], "list")
  expect_named(env$param[[3]])
  
  input <- "list(1,2,3)"
  expect_error(
    mrgsolve:::PARAM(x = input, as_object = TRUE, env = env, pos = 3), 
    "the returned object must have names"
  )
  
  expect_null(env$param[[8]])  
  env$ENV$parameters <- list(mm = 1, nn = 2)
  ans <- mrgsolve:::PARAM(x = input, object = "parameters", env = env, pos = 8)
  expect_is(env$param[[8]], "list")
  expect_named(env$param[[8]])
  
  expect_error(
    mrgsolve:::PARAM(x = "123", object = "parameters", as_object = TRUE), 
    "cannot have both @object and @as_object in a block"
  )
})

test_that("parse content using low-level handlers - THETA", {
  build <- new_test_build()
  env <- mrgsolve:::parse_env(vector(mode = "list", length = 20), build = build)
  sup <- suppressMessages  
  
  input <- "list(1,2,3)"
  expect_error(
    sup(mrgsolve:::THETA(x = input, as_object = TRUE)), 
    "the returned object was the wrong type"
  )
  input <- "c(3,4,5,6)"
  ans <- mrgsolve:::THETA(x = input, as_object = TRUE, env = env, pos = 10)
  expect_is(env$param[[10]], "list")
  expect_named(env$param[[10]])
  
  expect_null(env$param[[2]])  
  env$ENV$thetas <- c(9,8,7,6,5)
  ans <- mrgsolve:::THETA(x = "", object = "thetas", env = env, pos = 2)
  expect_is(env$param[[2]], "list")
  expect_named(env$param[[2]])
  
  expect_error(
    mrgsolve:::THETA(x = "123", object = "parameters", as_object = TRUE), 
    "cannot have both @object and @as_object in a block"
  )
})

test_that("parse content using low-level handlers - CMT", {
  build <- new_test_build()
  env <- mrgsolve:::parse_env(vector(mode = "list", length = 20), build = build)
  sup <- suppressMessages  
  
  input <- "c(2,2,3)"
  expect_error(
    sup(mrgsolve:::CMT(x = input, as_object = TRUE)), 
    "the returned object was the wrong type"
  )
  input <- "letters[1:3]"
  ans <- mrgsolve:::CMT(x = input, as_object = TRUE, env = env, pos = 8)
  expect_is(env$init[[8]], "numeric")
  expect_named(env$init[[8]])
  
  expect_null(env$param[[2]])  
  env$ENV$compartments <- letters[8:12]
  ans <- mrgsolve:::CMT(x = "", object = "compartments", env = env, pos = 2)
  expect_is(env$init[[2]], "numeric")
  expect_named(env$init[[2]])
  
  expect_error(
    mrgsolve:::CMT(x = "123", object = "parameters", as_object = TRUE), 
    "cannot have both @object and @as_object in a block"
  )
})

test_that("parse content using low-level handlers - INIT", {
  build <- new_test_build()
  env <- mrgsolve:::parse_env(vector(mode = "list", length = 20), build = build)
  sup <- suppressMessages  
  
  input <- "c(2,2,3)"
  expect_error(
    sup(mrgsolve:::INIT(x = input, as_object = TRUE)), 
    "the returned object was the wrong type"
  )
  
  input <- "list(z = 5, w = 8, h = 100)"
  ans <- mrgsolve:::INIT(x = input, as_object = TRUE, env = env, pos = 8)
  expect_is(env$init[[8]], "list")
  expect_named(env$init[[8]])
  
  expect_null(env$init[[2]])  
  env$ENV$initials <- list(u = 9, z = 10, y = 99)
  ans <- mrgsolve:::INIT(x = input, object = "initials", env = env, pos = 2)
  expect_is(env$init[[2]], "list")
  expect_named(env$init[[2]])
  
  expect_error(
    mrgsolve:::INIT(x = "123", object = "parameters", as_object = TRUE), 
    "cannot have both @object and @as_object in a block"
  )
})

test_that("parse content using low-level handlers - OMEGA, SIGMA", {
  build <- new_test_build()
  env <- mrgsolve:::parse_env(vector(mode = "list", length = 20), build = build)
  sup <- suppressMessages  
  
  input <- "c(1,2,3)"
  expect_error(
    sup(mrgsolve:::HANDLEMATRIX(x = input, as_object = TRUE)), 
    "the returned object was the wrong type"
  )
  
  input <- "matrix(0, 6, 6)"
  ans <- mrgsolve:::HANDLEMATRIX(
    oclass = "omegalist", type = "omega",
    x = input, as_object = TRUE, env = env, pos = 8
  )
  expect_is(env$omega[[8]], "matlist")
  
  input <- "
  m <- matrix(0, 6, 6)
  dimnames(m) <- list(letters[1:6], NULL)
  m
  "
  ans <- mrgsolve:::HANDLEMATRIX(
    oclass = "omegalist", type = "omega",
    x = input, as_object = TRUE, env = env, pos = 4
  )
  expect_is(env$omega[[4]], "matlist")
  ans <- labels(env$omega[[4]])[[1]]
  expect_equal(ans, letters[1:6])
  
  input <- ""
  expect_null(env$omega[[12]])
  dnames <-c("j", "k", "l")
  env$ENV$omga <- matrix(0, 3, 3, dimnames = list(dnames, dnames))
  ans <- mrgsolve:::HANDLEMATRIX(
    oclass = "omegalist", type = "omega",
    x = input, object = "omga", env = env, pos = 12
  )
  expect_is(env$omega[[12]], "matlist")
  ans <- labels(env$omega[[12]])[[1]]
  expect_equal(ans, dnames)
  
  expect_error(
    mrgsolve:::HANDLEMATRIX(x = "123", object = "parameters", as_object = TRUE), 
    "cannot have both @object and @as_object in a block"
  )
})

test_that("autodec parsing", {
  x <- mrgsolve:::autodec_find("a = 1;")  
  expect_equal(x, "a")
  x <- mrgsolve:::autodec_find("a=1;")  
  expect_equal(x, "a")
  x <- mrgsolve:::autodec_find("double a_2 = 1;")
  expect_equal(x, "a_2")
  x <- mrgsolve:::autodec_find("if(x == 2) y = 3;")  
  expect_equal(x, "y")
  x <- mrgsolve:::autodec_find("a == 1;")  
  expect_equal(x, character(0))
  x <- mrgsolve:::autodec_find("if(NEWIND <= 1 ) {")  
  expect_equal(x, character(0))
  x <- mrgsolve:::autodec_find("if(EVID >= 1 ) {")  
  expect_equal(x, character(0))
  x <- mrgsolve:::autodec_find("if(TIME != 1 ) {")  
  expect_equal(x, character(0))
  x <- mrgsolve:::autodec_find("self.foo = 1;")
  expect_equal(x, character(0))
  code <- strsplit(split = "\n", '
    double a = 2;
    b = 3;
    if(c==2) d = 1;
    b=(123);
    k = 
  ')[[1]]
  x <- mrgsolve:::autodec_vars(code)
  expect_equal(x, c("a", "b", "d", "k"))
  
})

test_that("autodec models", {
  code <- ' 
  [ plugin ] autodec
  [ param ] a = 1, b = 2
  '
  expect_s4_class(mod <- mcode("autodec2", code, compile = FALSE), "mrgmod")
  l <- as.list(mod)
  expect_equal(nrow(l$cpp_variables), 0)
  
  code <- ' 
  [ plugin ] autodec
  [ param ] a = 1, b = 2
  [ main ] 
  double c = 3;
  '
  expect_s4_class(mod <- mcode("autodec3", code, compile = FALSE), "mrgmod")
  l <- as.list(mod)
  expect_equal(l$cpp_variables$var, "c")
  
  code <- ' 
  [ plugin ] autodec
  [ param ] a = 1, b = 2
  [ main ] 
  double c = 3;
  d = 4;
  '
  expect_s4_class(mod <- mcode("autodec4", code, compile = FALSE), "mrgmod")
  l <- as.list(mod)
  expect_equal(l$cpp_variables$var, c("c", "d"))
  
  code <- '
  [ param ] tvcl = 1, tvvc = 2
  [ cmt ] GUT CENT
  [ plugin ] autodec
  [ main ] 
  cl = tvcl;
  v2 = tvvc;
  ka = 1;
  F_CENT = 1;
  if(NEWIND <=1 ) {
    D_CENT = 4;  
  }
  double F1 = 0.9;
  [ table ] 
  double err = EPS(1);
  CP = cent/v2;
  '
  mod <- mcode("autodec5", code, compile = FALSE)
  cpp <- as.list(mod)$cpp_variables
  expect_equal(cpp$var, c("F1", "err", "cl", "v2", "ka", "CP"))
  expect_equal(cpp$context, c("main", "table", rep("auto", 4)))
  
})

test_that("autodec models with nm-vars", {
  code <- '
  [ param ] tvcl = 1, tvvc = 2
  [ cmt ] GUT CENT
  [ plugin ] autodec nm-vars
  [ main ] 
  double km = 2.5;
  cl = tvcl;
  v2 = tvvc;
  ka = 1;
  F_GUT = 1.2;
  F1 = 1.2;
  if(NEWIND<=1) {
    D2 = 4;  
  }
  ALAG2 = 0.2;
  A_0(2) = 5;
  [ table ] 
  double err = EPS(1);
  CP = cent/v2;
  [ ode ] 
  DADT(1) = 0;
  DADT(2) = 1; 
  '
  mod <- mcode("autodec5", code, compile = FALSE)
  cpp <- as.list(mod)$cpp_variables
  expect_equal(cpp$var, c("km","err", "cl", "v2", "ka", "CP"))
  expect_equal(cpp$context, c("main", "table",  rep("auto", 4)))
})

test_that("autodec variables can be skipped", {
  code <- '
  [ plugin ] autodec
  [ env ] MRGSOLVE_AUTODEC_SKIP = "a, c"
  [ main ] 
  a = 1; 
  b = 2; 
  c = 3; 
  d = 4;
  double e = 5;
  '
  mod <- mcode("autodec-skip", code, compile = FALSE)
  cpp <- as.list(mod)$cpp_variables
  expect_equal(cpp$var, c("e", "b", "d"))
})

test_that("tagged parameter blocks", {
  code <- "$PARAM @input \n CL = 5"
  x <- mcode("tag-1", code, compile = FALSE)
  expect_equal(names(param(x)), "CL")
  tagdf <- x@shlib$param_tag
  expect_is(tagdf, "data.frame")
  expect_equal(names(tagdf), c("name", "tag"))
  expect_equal(tagdf$name, "CL")
  expect_equal(tagdf$tag, "input")
  
  code <- "$PARAM @tag foo, bar par @input \n V2 = 5"
  x <- mcode("tag-2", code, compile = FALSE)
  expect_equal(names(param(x)), "V2")
  tagdf <- x@shlib$param_tag
  expect_equal(nrow(tagdf), 4)
  expect_equal(tagdf$name, rep("V2", 4))
  expect_equal(tagdf$tag, c("input", "foo", "bar", "par"))
  
  code <- "$PARAM @tag foo, bar \n V2 = 5, CL = 3"
  x <- mcode("tag-3", code, compile = FALSE)
  tagdf <- x@shlib$param_tag
  check <- expand.grid(
    name = c("V2", "CL"), 
    tag = c("foo", "bar"), 
    stringsAsFactors = FALSE
  )
  expect_equal(tagdf, check)
})

test_that("INPUT block", {
  code <- "$INPUT CL = 1, V2 = 2"
  x <- mcode("input-1", code, compile = FALSE)
  expect_equal(names(param(x)), c("CL", "V2"))
  tagdf <- x@shlib$param_tag
  expect_is(tagdf, "data.frame")
  expect_equal(names(tagdf), c("name", "tag"))
  expect_equal(tagdf$name, c("CL", "V2"))
  expect_equal(tagdf$tag, rep("input", 2))
})

test_that("Reserve names in cpp dot gh-1159", {
  # clash with parameter
  code <- '
  $param cl = 2, v = 2, d = 5
  $cmt a b c
  $main 
  foo.d = 2;
  '  
  expect_error(
    mcode("cpp-dot-1", code, compile = FALSE), 
    regexp = "d (parameter)", 
    fixed = TRUE
  )
  
  # clash with compartment
  code <- '
  $param cl = 2, v = 2
  $cmt a b c
  $ode 
  foo.a = 2;
  '
  
  expect_error(
    mcode("cpp-dot-2", code, compile = FALSE), 
    regexp = "a (compartment)", 
    fixed = TRUE
  )
  
  # clash with omega
  code <- '
  $param cl = 2, v = 2
  $cmt a b c
  $omega @labels foo
  1
  $table 
  foo.e = 2;
  '
  
  expect_error(
    mcode("cpp-dot-3", code, compile = FALSE), 
    regexp = "foo (eta label)", 
    fixed = TRUE
  )
  
  # clash with sigma
  code <- '
  $param cl = 2, v = 2
  $cmt a b c
  $sigma @labels bar
  1
  $preamble
  foo.bar = 2;
  '
  
  expect_error(
    mcode("cpp-dot-4", code, compile = FALSE), 
    regexp = "bar (eps label)", 
    fixed = TRUE
  )
  
  # some names are checked in the object
  code <- '
  $param rate = 2
  $main
  ev.rate = 2;
  '
  
  expect_error(
    mcode("cpp-dot-5", code, compile = FALSE), 
    regexp = "Reserved words in model names: rate", 
    fixed = TRUE
  )
})

test_that("Skip cpp dot check gh-1159", {
  temp <- '$param {param}\n$main\n{main};\n$env MRGSOLVE_CPP_DOT_SKIP="foo"'
  
  param <- "cl = 1, foo = 3, vc = 5"
  main <- "double b = 5;\nfoo.bar = true;"
  table <- "true;"
  code <- glue::glue(temp)
  
  expect_s4_class(mcode("cpp-dot-skip-1", code, compile = FALSE), "mrgmod")
  
  temp <- '$param {param}\n$main\n{main};\n$env MRGSOLVE_CPP_DOT_SKIP="foo"'
  param <- "cl = 1, foo = 3, bar = 2, vc = 5"
  main <- "double b = 5;\nfoo.bar = true;"
  table <- "true;"
  code <- glue::glue(temp)
  
  expect_error(
    mcode("cpp-dot-skip-2", code, compile = FALSE), 
    regexp = "bar (parameter)", 
    fixed = TRUE
  )
  
  check <- try(mcode("cpp-dot-skip-3", code, compile = FALSE), silent = TRUE)
  
  expect_match(check, "bar (parameter)", fixed = TRUE)
  expect_no_match(check, "foo (parameter)", fixed = TRUE)
  
  temp <- '$param {param}\n$main\n{main};\n$env MRGSOLVE_CPP_DOT_SKIP="foo,bar"'
  param <- "cl = 1, foo = 3, bar = 2, vc = 5"
  main <- "double b = 5;\nfoo.bar = true;"
  table <- "true;"
  code <- glue::glue(temp)
  
  expect_s4_class(mcode("cpp-dot-skip-4", code, compile = FALSE), "mrgmod")
})
