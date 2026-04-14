# Copyright (C) 2013 - 2026  Metrum Research Group
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

test_that("multiple blocks allowed or not allowed", {
  
  for(bl in mrgsolve:::block_list_single) {
    code <- glue::glue("${bl} end = 5\n${bl} delta = 1\n$PARAM x = 3")  
    model <- glue::glue("test-multiple-{tolower(bl)}")
    expect_error(
      mcode(model, code, compile = FALSE), 
      "Multiple blocks found"
    )
  }
  
  code <- "$PLUGIN nm-vars\n$PLUGIN autodec evtools\n$PARAM x = 3"
  expect_silent(
    mod <- mcode("test-multiple-plugin", code, compile = FALSE)
  )
  expect_is(mod, "mrgmod")
  
  code <- "$CMT a\n$ODE dxdt_a = 3\n$ODE b = 55\n$PARAM x = 3"
  expect_silent(
    mod <- mcode("test-multiple-ode", code, compile = FALSE)
  )
  expect_is(mod, "mrgmod")
  
  code <- "$TABLE x\n$TABLE y = 55\n$PARAM x = 3\n y = 10"
  expect_silent(
    mod <- mcode("test-multiple-table", code, compile = FALSE)
  )
  expect_is(mod, "mrgmod")
})

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
  expect_equal(x, character(0))
  x <- mrgsolve:::autodec_find("int i_2 = 1;")
  expect_equal(x, character(0))
  x <- mrgsolve:::autodec_find("bool b_2 = false;")
  expect_equal(x, character(0))
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
  x <- mrgsolve:::autodec_find("if(TIME != 1 ) {ccc = 11;")  
  expect_equal(x, "ccc")
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
  expect_equal(x, c("b", "d", "k"))
  
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
  evt::ev dose = evt::infuse(100, 1);
  fo.bar = 2;
  [ ode ] 
  DADT(1) = 0;
  DADT(2) = 1; 
  '
  mod <- mcode("autodec5", code, compile = FALSE)
  cpp <- as.list(mod)$cpp_variables
  expect_equal(cpp$var, c("km", "err", "cl", "v2", "ka", "CP"))
  expect_equal(cpp$context, c("main", "table",  rep("auto", 4)))
  
  # No prefixes
  code <- '
  $PLUGIN autodec
  $MAIN 
  a = 1; 
  b = 2; 
  c = 3;
  '
  mod <- mcode("autodec5b", code, compile = FALSE)
  cpp <- as.list(mod)$cpp_variables
  expect_equal(cpp$var, c("a", "b", "c"))
  
  # Nothing to find
  code <- '
  $PLUGIN autodec
  $MAIN 
  if(NEWIND <= 1) {
   // commented out
  }
  '
  mod <- mcode("autodec5c", code, compile = FALSE)
  cpp <- as.list(mod)$cpp_variables
  expect_equal(nrow(cpp), 0)
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

test_that("Invalid item in $SET generates error", {
  code <- "$SET end = 25, kyle = 2"
  expect_error(
    mcode("dollar-set-item-check-1", code),
    "The $SET block cannot handle this item", 
    fixed = TRUE
  )
  code <- "$SET end = 25, kyle = 2, zip = 55455"
  expect_error(
    mcode("dollar-set-item-check-2", code),
    "The $SET block cannot handle these items", 
    fixed = TRUE
  )
  code <- "$SET a = 2"  # ambiguous partial match
  expect_error(
    mcode("dollar-set-item-check-3", code),
    "The $SET block cannot handle this item",
    fixed = TRUE
  )
})

# -----------------------------------------------------------------------------

test_that("modelsplit and unsplit leaves code unchanged", {
  code <- '
  This is an unparsed header. 

  // A comment
  
  $CMT @number 2

  $PARAM CL = 1
  VC = 2, KA = 1.2

  $MAIN
  double KE = CL/VC;

  $ODE
  DADT(1) = -KA * A(1)

  DADT(2) = KA * A(1)  - KE * A(2); // central compartment 
  
  $CAPTURE KA KE
  '
  code <- strsplit(code, "\n")[[1]]
  x <- modelsplit(code, split = FALSE)
  y <- modelunsplit(x)
  expect_identical(code, y)
})


# -----------------------------------------------------------------------------

test_that("convert_pow passes through lines without **", {
  expect_equal(convert_pow("double x = a + b;"), "double x = a + b;")
  expect_equal(convert_pow("y = 1.0;"), "y = 1.0;")
  expect_equal(convert_pow("// just a comment"), "// just a comment")
  expect_equal(convert_pow("x = a ^ b;"), "x = a ^ b;")
})

test_that("convert_pow handles simple base ** exponent", {
  expect_equal(convert_pow("x = a**2;"), "x = pow(a, 2);")
  expect_equal(convert_pow("x = a**b;"), "x = pow(a, b);")
  expect_equal(convert_pow("x = 2**3;"), "x = pow(2, 3);")
})

test_that("convert_pow handles floating point exponents", {
  expect_equal(convert_pow("x = a**0.75;"), "x = pow(a, 0.75);")
  expect_equal(convert_pow("x = a**1.5e-3;"), "x = pow(a, 1.5e-3);")
})

test_that("convert_pow handles parenthesized bases", {
  expect_equal(convert_pow("(WT/70)**0.75"), "pow(WT/70, 0.75)")
  expect_equal(convert_pow("(a+b)**2;"), "pow(a+b, 2);")
  expect_equal(convert_pow("(a*b)**c;"), "pow(a*b, c);")
})

test_that("convert_pow handles parenthesized exponents", {
  expect_equal(convert_pow("a**(b+c);"), "pow(a, b+c);")
  expect_equal(convert_pow("x = a**(1/3);"), "x = pow(a, 1/3);")
})

test_that("convert_pow handles negative exponents", {
  expect_equal(convert_pow("a**-2;"), "pow(a, -2);")
  expect_equal(convert_pow("a**-THETA(5);"), "pow(a, -THETA(5));")
})

test_that("convert_pow handles multiple ** in one line", {
  expect_equal(
    convert_pow("x = a**2 + b**3;"),
    "x = pow(a, 2)+pow(b, 3);"
  )
})

test_that("convert_pow handles nested **", {
  expect_equal(
    convert_pow("x = (a**2)**3;"),
    "x = pow(pow(a, 2), 3);"
  )
  expect_equal(
    convert_pow("x = 2**3**2;"),
    "x = pow(2, pow(3, 2));"
  )
})

test_that("prededence of ** over / and *", {
  expect_equal(convert_pow("10/5**2"), "10/pow(5, 2)")
  expect_equal(convert_pow("10*5**2"), "10*pow(5, 2)")
  expect_equal(convert_pow("A / B**C / D**E"), "A/pow(B, C)/pow(D, E)")
})

test_that("convert_pow handles unary minus", {
  expect_equal(convert_pow("x = -a**2;"), "x = -pow(a, 2);")
  expect_equal(convert_pow("x = (-a)**2;"), "x = pow(-a, 2);")
  expect_equal(convert_pow("x = a**-2;"), "x = pow(a, -2);")
})

test_that("convert_pow handles function calls in expressions", {
  expect_equal(
    convert_pow("x = exp(a)**2;"),
    "x = pow(exp(a), 2);"
  )
  expect_equal(
    convert_pow("x = log(a+b)**c;"),
    "x = pow(log(a+b), c);"
  )
  expect_equal(
    convert_pow("x = (a+b)**exp(c+d);"),
    "x = pow(a+b, exp(c+d));"
  )
})

test_that("convert_pow preserves trailing semicolons", {
  expect_equal(convert_pow("x = a**2;"), "x = pow(a, 2);")
  expect_equal(convert_pow("x = a**2"), "x = pow(a, 2)")
})

test_that("convert_pow ignores trailing // comment", {
  expect_equal(convert_pow("x = a**2; // comment"), "x = pow(a, 2);")
  expect_equal(convert_pow("x = a**2 // comment"), "x = pow(a, 2)")
})

test_that("convert_pow works on character vectors", {
  code <- c("x = a**2;", "y = b + c;", "z = (d/e)**0.5;")
  expected <- c("x = pow(a, 2);", "y = b + c;", "z = pow(d/e, 0.5);")
  expect_equal(convert_pow(code), expected)
})

test_that("convert_pow returns non-character input unchanged", {
  expect_equal(convert_pow(42), 42)
  expect_null(convert_pow(NULL))
})

test_that("convert_pow handles pow() fine", {
  code <- "x = pow(a, b + 5);"
  expect_equal(convert_pow(code), code)
  
  code <- "x = pow(a, b + 5) + 2.23**9.98;"
  expect_equal(convert_pow(code), "x = pow(a, b+5)+pow(2.23, 9.98);") 
})

test_that("convert_pow returns original string and warns when parsing fails", {
  bad <- "x = a ** ** b;"
  expect_warning(
    expect_equal(convert_pow(bad), bad),
    regexp = "Could not convert \\*\\*"
  )
})

test_that("convert_pow warning includes block name when provided", {
  bad <- "x = a ** ** b;"
  expect_warning(
    convert_pow(bad, block = "MAIN"),
    regexp = "\\$MAIN block"
  )
})

test_that("convert_pow warning omits block clause when block is empty", {
  bad <- "x = a ** ** b;"
  w <- tryCatch(convert_pow(bad), warning = function(w) conditionMessage(w))
  expect_false(grepl("block", w))
})

test_that("convert_pow handles PK/PD style expressions", {
  # allometric scaling
  expect_equal(
    convert_pow("CL = TVCL * (WT/70)**0.75;"),
    "CL = TVCL*pow(WT/70, 0.75);"
  )
  # theta-parameterized exponent
  expect_equal(
    convert_pow("CL = TVCL * (WT/70)**THETA(2);"),
    "CL = TVCL*pow(WT/70, THETA(2));"
  )
  # Emax model with THETA(.) Hill coefficient
  expect_equal(
    convert_pow("EFFECT = EMAX * CP**THETA(5) / (EC50**THETA(5) + CP**THETA(5));"),
    "EFFECT = EMAX*pow(CP, THETA(5))/(pow(EC50, THETA(5))+pow(CP, THETA(5)));"
  )
  # sigmoid Emax
  expect_equal(
    convert_pow("EFFECT = EMAX * CP**HILL / (EC50**HILL + CP**HILL);"),
    "EFFECT = EMAX*pow(CP, HILL)/(pow(EC50, HILL)+pow(CP, HILL));"
  )
  # multiple ** with function calls and allometric scaling
  expect_equal(
    convert_pow("CL = THETA(1)*exp(ETA(1))*THETA(6)**SEX*(WT/70)**THETA(7);"),
    "CL = THETA(1)*exp(ETA(1))*pow(THETA(6), SEX)*pow(WT/70, THETA(7));"
  )
  # ODE with nested ** (generalized logistic growth)
  expect_equal(
    convert_pow("dxdt_TS = kge * TS * (1 - TS/TSmax) / (1 + (kge/kgl * TS)**psi)**(1/psi)"),
    "dxdt_TS = kge*TS*(1-TS/TSmax)/pow(1+pow(kge/kgl*TS, psi), 1/psi)"
  )
  # residual error
  expect_equal(
    convert_pow("W2 = SQRT(THETA(9)**2*IPRED2**2 + THETA(10)**2)"),
    "W2 = SQRT(pow(THETA(9), 2)*pow(IPRED2, 2)+pow(THETA(10), 2))"
  )
  # Weibull hazard
  expect_equal(
    convert_pow("HAZT = BETA * (EDRUGT**BETA) * (T**(BETA - 1)) * EXP(covar);"),
    "HAZT = BETA*pow(EDRUGT, BETA)*pow(T, BETA-1)*EXP(covar);"
  )
  # Some crazy bone model stuff
  expect_equal(
    convert_pow("T85 = T84 + (T77 - T84)*(A(9)**T80) / ((A(9)**T80) + (T81**T80))"), 
    "T85 = T84+(T77-T84)*pow(A(9), T80)/(pow(A(9), T80)+pow(T81, T80))"
  )
})

test_that("convert_pow handles expressions with no assignment", {
  expect_equal(convert_pow("a**2"), "pow(a, 2)")
  expect_equal(convert_pow("(WT/70)**0.75"), "pow(WT/70, 0.75)")
})

test_that("convert_pow doesn't convert /** at start of c-style comment", {
  code <- '
  $GLOBAL 
  /** 
  This model is incredible.
  */
  double foo = 123;
  $PARAM THETA1 = 1
  $MAIN
  double CL = THETA1 * (WT/123) ** 0.75;
  '
  lines <- modelparse(code, split = TRUE)
  x <- lapply(lines, convert_pow)
  x <- lapply(x, trimws)
  expect_equal(x$MAIN, "double CL = THETA1*pow(WT/123, 0.75);")
  expect_equal(x$GLOBAL[1], "/**")
  expect_equal(x$GLOBAL[3], "*/")
  expect_equal(x$PARAM, "THETA1 = 1")
})

test_that("convert_pow will convert and maybe warn inside c-style comment", {
  code <- c("/**", "This model is incredible. 3**4", "*/")
  expect_warning(convert_pow(code), "Could not convert **", fixed = TRUE)
})

test_that("Convert pow in  PREAMBLE, MAIN, ODE, TABLE, EVENT", {
  code_convert_pow_1 <- '
  $PARAM a = 1.0, b = 2, c = 3
  $PREAMBLE double d = a**b;
  $MAIN d = a**b;
  $ODE @!audit \nd = a**b;
  $TABLE d = a**b; 
  $EVENT d = a**b;
  $CMT A B C 
  '
  mod <- mcode('power', code_convert_pow_1, compile = FALSE)
  x <- readLines(mod@shlib$source)
  x <- x[grepl("d =", x, fixed = TRUE)]
  expect_length(x, 5)
  expect_match(x, "pow(a, b)", fixed = TRUE, all = TRUE)
})

test_that("Convert pow in  PREAMBLE, PRED", {
  code_convert_pow_2 <- '
  $PARAM a = 1.0, b = 2, c = 3
  $PREAMBLE double d = a**b;
  $PRED d = a**b;
  '
  mod <- mcode('power', code_convert_pow_2, compile = FALSE)
  x <- readLines(mod@shlib$source)
  x <- x[grepl("d =", x, fixed = TRUE)]
  expect_length(x, 2)
  expect_match(x, "pow(a, b)", fixed = TRUE, all = TRUE)
})

test_that("convert_pow handles comparison operators in exponent", {
  expect_equal(convert_pow("THETA(2)**(RACE==3)"), "pow(THETA(2), RACE==3)")
  expect_equal(convert_pow("THETA(2)**(RACE!=3)"), "pow(THETA(2), RACE!=3)")
  expect_equal(convert_pow("THETA(2)**(RACE>=3)"), "pow(THETA(2), RACE>=3)")
  expect_equal(convert_pow("THETA(2)**(RACE<=3)"), "pow(THETA(2), RACE<=3)")
  expect_equal(convert_pow("THETA(2)**(RACE>3)"),  "pow(THETA(2), RACE>3)")
  expect_equal(convert_pow("THETA(2)**(RACE<3)"),  "pow(THETA(2), RACE<3)")
})

test_that("convert_pow handles comparison operators in assignment prefix", {
  expect_equal(
    convert_pow("if(a==3) b = THETA(2)**RACE"),
    "if(a==3) b = pow(THETA(2), RACE)"
  )
  expect_equal(
    convert_pow("if(a>=3) b = THETA(2)**(RACE)"),
    "if(a>=3) b = pow(THETA(2), RACE)"
  )
  expect_equal(
    convert_pow("if(a<=3) b = THETA(2)**RACE"),
    "if(a<=3) b = pow(THETA(2), RACE)"
  )
  expect_equal(
    convert_pow("if(a!=3) b = THETA(2)**(RACE)"),
    "if(a!=3) b = pow(THETA(2), RACE)"
  )
})

test_that("convert_pow handles logical operators in exponent", {
  expect_equal(
    convert_pow("THETA(2)**(RACE==1||RACE==2)"),
    "pow(THETA(2), RACE==1||RACE==2)"
  )
  expect_equal(
    convert_pow("THETA(2)**(SEX==1&&AGE>=18)"),
    "pow(THETA(2), SEX==1&&AGE>=18)"
  )
})

test_that("convert_pow handles ** in if() condition", {
  expect_equal(
    convert_pow("if(x**2 > 5) d = 2**3;"),
    "if(pow(x, 2)>5) d = pow(2, 3);"
  )
  expect_equal(
    convert_pow("if(WT**2 >= 100) CL = THETA(1)**0.75;"),
    "if(pow(WT, 2)>=100) CL = pow(THETA(1), 0.75);"
  )
  # condition has **, rhs does not
  expect_equal(
    convert_pow("if(x**2 > 5) d = 1;"),
    "if(pow(x, 2)>5) d = 1;"
  )
  # rhs has **, condition does not
  expect_equal(
    convert_pow("if(x > 5) d = 2**3;"),
    "if(x > 5) d = pow(2, 3);"
  )
})

test_that("environment variable suppresses convert pow", {
  code <- '
  $ENV MRGSOLVE_CONVERT_POW <- TRUE
  $PK
  double a = 2**4;
  '
  mod <- mcode("supp-pow-1", code, compile = FALSE)
  code <- readLines(mod@shlib$source)
  code <- code[grepl("a = ", code, fixed = TRUE)]
  code <- trimws(code)
  expect_equal(code, "a = pow(2, 4);")
  
  code <- '
  $ENV MRGSOLVE_CONVERT_POW <- FALSE
  $PK
  double a = 2**4;
  '
  mod <- mcode("supp-pow-2", code, compile = FALSE)
  code <- readLines(mod@shlib$source)
  code <- code[grepl("a = ", code, fixed = TRUE)]
  code <- trimws(code)
  expect_equal(code, "a = 2**4;")
})

# warn_int_div --------------------------------------------------

test_that("warn_int_div warns for simple integer division", {
  expect_warning(
    warn_int_div("CL = 3/4 * THETA(1);"),
    "3/4"
  )
  expect_warning(
    warn_int_div("V = 1/2;"),
    "1/2"
  )
})

test_that("warn_int_div warns for negative numbers", {
  expect_warning(warn_int_div("CL = -3/4 * THETA(1);"), "-3/4")
  expect_warning(warn_int_div("CL = 3/-4 * THETA(1);"), "3/-4")
  expect_warning(warn_int_div("CL = -3/-4 * THETA(1);"), "-3/-4")
})

test_that("warn_int_div strips parens", {
  expect_warning(warn_int_div("CL = (3)/(4) * THETA(1);"), "3/4")
  expect_warning(warn_int_div("CL = 3/(-4) * THETA(1);"), "3/-4")
  expect_warning(warn_int_div("CL = (-3)/(-4) * THETA(1);"), "-3/-4")
})

test_that("warn_int_div is silent when no integer division", {
  expect_no_warning(warn_int_div("CL = 0.5/2;"))
  expect_no_warning(warn_int_div("x = a/b;"))
  expect_no_warning(warn_int_div("y = 2.0/4;"))
  expect_no_warning(warn_int_div("z = THETA(1)/THETA(2);"))
})

test_that("warn_int_div warns for each instance on a line", {
  w <- character(0)
  withCallingHandlers(
    warn_int_div("F1 = 1/4 + 3/8;"),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 2)
  expect_true(any(grepl("1/4", w)))
  expect_true(any(grepl("3/8", w)))
})

test_that("warn_int_div includes block name in warning", {
  expect_warning(
    warn_int_div("CL = 3/4;", block = "PK"),
    "\\$PK"
  )
})

test_that("warn_int_div returns input unchanged", {
  x <- c("CL = 3/4;", "V = a/b;")
  expect_equal(
    suppressWarnings(warn_int_div(x)),
    x
  )
})

test_that("warn_int_div passes through non-character input", {
  expect_no_warning(warn_int_div(42))
  expect_no_warning(warn_int_div(NULL))
})


test_that("warn_int_div detects integer division before line comment", {
  expect_warning(warn_int_div("1/2 // hey"), "1/2")
  expect_warning(warn_int_div("CL = 3/4; // comment"), "3/4")
  expect_no_warning(warn_int_div("x = a/b; // 1/2 is in the comment"))
})

test_that("environment variable suppresses int div warning", {
  code <- '
  $ENV MRGSOLVE_WARN_INT_DIV <- TRUE
  $PK
  double a = 3/4;
  '
  expect_warning(
    mcode("int-div-env-1", code, compile = FALSE), 
    "Integer division"
  )
  code <- '
  $ENV MRGSOLVE_WARN_INT_DIV <- FALSE
  $PK
  double a = 3/4;
  '
  expect_no_warning(
    mcode("int-div-env-2", code, compile = FALSE), 
  )
})

# convert_fort_if -----------------------------------------------------

fi <- convert_fort_if

test_that("convert_fort_if: block form with Fortran operator", {
  x <- c("IF(WT.GE.70) THEN", "  CL = THETA(1)", "ENDIF")
  expect_equal(fi(x), c("if(WT >= 70) {", "  CL = THETA(1)", "}"))
  x <- c("IF(WT.GE.70)THEN", "  CL = THETA(1)", "ENDIF")
  expect_equal(fi(x), c("if(WT >= 70) {", "  CL = THETA(1)", "}"))
})

test_that("convert_fort_if: all Fortran relational operators", {
  expect_equal(fi("IF(A.GE.B) THEN"), "if(A >= B) {")
  expect_equal(fi("IF(A.LE.B) THEN"), "if(A <= B) {")
  expect_equal(fi("IF(A.GT.B) THEN"), "if(A > B) {")
  expect_equal(fi("IF(A.LT.B) THEN"), "if(A < B) {")
  expect_equal(fi("IF(A.EQ.B) THEN"), "if(A == B) {")
  expect_equal(fi("IF(A.NE.B) THEN"), "if(A != B) {")
  expect_equal(fi("IF(A/=B) THEN"),   "if(A != B) {")
})

test_that("convert_fort_if: logical operators in condition", {
  expect_equal(fi("IF(A.GT.0.AND.B.LT.1) THEN"), "if(A > 0 && B < 1) {")
  expect_equal(fi("IF(A.LT.0.OR.B.GT.1) THEN"),  "if(A < 0 || B > 1) {")
})

test_that("convert_fort_if: spaces around Fortran operators are normalized", {
  x <- c("IF(WT .GE. 70) THEN", "  CL = THETA(1)", "ENDIF")
  expect_equal(fi(x), c("if(WT >= 70) {", "  CL = THETA(1)", "}"))
  expect_equal(fi("IF(A .AND. B .GT. 0) THEN"), "if(A && B > 0) {")
})

test_that("convert_fort_if: single-line form", {
  expect_equal(fi("IF(WT.GE.70) CL = THETA(1)"), "if(WT >= 70) CL = THETA(1)")
  expect_equal(fi("IF(A.GT.0) x = 1"),            "if(A > 0) x = 1")
})

test_that("convert_fort_if: if/else block", {
  x <- c("IF(WT.GE.70) THEN", "  CL = 2", "ELSE", "  CL = 1", "ENDIF")
  expect_equal(fi(x), c("if(WT >= 70) {", "  CL = 2", "} else {", "  CL = 1", "}"))
})

test_that("convert_fort_if: ELSEIF (no space)", {
  x <- c("IF(A.GT.0) THEN", "  x = 1", "ELSEIF(A.LT.0) THEN", "  x = -1", "ENDIF")
  expect_equal(fi(x), c("if(A > 0) {", "  x = 1", "} else if(A < 0) {", "  x = -1", "}"))
})

test_that("convert_fort_if: ELSE IF (two words)", {
  x <- c("IF(A.GT.0) THEN", "  x = 1", "ELSE IF(A.LT.0) THEN", "  x = -1", "ENDIF")
  expect_equal(fi(x), c("if(A > 0) {", "  x = 1", "} else if(A < 0) {", "  x = -1", "}"))
})

test_that("convert_fort_if: END IF (two words)", {
  x <- c("IF(A.GT.0) THEN", "  x = 1", "END IF")
  expect_equal(fi(x), c("if(A > 0) {", "  x = 1", "}"))
})

test_that("convert_fort_if: case insensitive keywords", {
  expect_equal(fi("if(wt.ge.70) then"), "if(wt >= 70) {")
  expect_equal(fi("endif"),             "}")
  expect_equal(fi("else"),              "} else {")
})

test_that("convert_fort_if: indentation preserved", {
  expect_equal(fi("  IF(A.GT.0) THEN"), "  if(A > 0) {")
  expect_equal(fi("  ENDIF"),           "  }")
})


test_that("convert_fort_if: nested parens in condition", {
  expect_equal(fi("IF(F(WT).GE.G(AGE)) THEN"), "if(F(WT) >= G(AGE)) {")
})

test_that("convert_fort_if: comment lines pass through unchanged", {
  expect_equal(fi("// IF(X.GT.0) THEN"), "// IF(X.GT.0) THEN")
})

test_that("convert_fort_if: Fortran ops on non-IF lines pass through unchanged", {
  expect_equal(fi("  CL = CL * (SEX.EQ.1)"), "  CL = CL * (SEX.EQ.1)")
  expect_equal(fi("CL = THETA(1).GE.0"),      "CL = THETA(1).GE.0")
})


test_that("convert_fort_if: non-character input passes through", {
  expect_equal(fi(42),   42)
  expect_null(fi(NULL))
})

test_that("convert_fort_if: compound conditions with .AND.", {
  expect_equal(fi("IF(A.GE.B.AND.D.GT.C) THEN"),   "if(A >= B && D > C) {")
  expect_equal(fi("IF(A.GE.B.AND.D.GT.C) CL = 1"), "if(A >= B && D > C) CL = 1")
})

test_that("convert_fort_if: compound conditions with .OR.", {
  expect_equal(fi("IF(A.GE.B.OR.C.LE.D) THEN"), "if(A >= B || C <= D) {")
})

test_that("convert_fort_if: three operators in condition", {
  expect_equal(fi("IF(A.GE.B.OR.C.LE.D.AND.E.NE.F) THEN"), "if(A >= B || C <= D && E != F) {")
})

test_that("convert_fort_if: float literals adjacent to operators", {
  expect_equal(fi("IF(A.GE.0.5.AND.B.LT.1.0) THEN"), "if(A >= 0.5 && B < 1.0) {")
  expect_equal(fi("IF(A.GT.1E-3.AND.B.LT.2) THEN"),  "if(A > 1E-3 && B < 2) {")
})



test_that("environment variable suppresses fortran if else conversion", {
  code <- '
  $ENV MRGSOLVE_CONVERT_FORT_IF <- TRUE
  $PK
  IF(A.EQ.2) B = 5;
  '
  mod <- mcode("supp-fort-if-1", code, compile = FALSE)
  code <- readLines(mod@shlib$source)
  code <- code[grepl("B = ", code, fixed = TRUE)]
  code <- trimws(code)
  expect_equal(code, "IF(A.EQ.2) B = 5;")
  
  code <- '
  $PLUGIN nm-vars
  $ENV MRGSOLVE_CONVERT_FORT_IF <- TRUE
  $PK
  IF(A.EQ.2) B = 5;
  '
  mod <- mcode("supp-fort-if-2", code, compile = FALSE)
  code <- readLines(mod@shlib$source)
  code <- code[grepl("B = ", code, fixed = TRUE)]
  code <- trimws(code)
  expect_equal(code, "if(A == 2) B = 5;")
  
  code <- '
  $PLUGIN nm-vars
  $ENV MRGSOLVE_CONVERT_FORT_IF <- FALSE
  $PK
  IF(A.EQ.2) B = 5;
  '
  mod <- mcode("supp-fort-if-3", code, compile = FALSE)
  code <- readLines(mod@shlib$source)
  code <- code[grepl("B = ", code, fixed = TRUE)]
  code <- trimws(code)
  expect_equal(code, "IF(A.EQ.2) B = 5;")
})
# convert_semicolons ---------------------------------------------------------

as_ <- mrgsolve:::convert_semicolons

test_that("convert_semicolons: adds semicolon to plain statement", {
  expect_equal(as_("CL = THETA(1) * exp(ETA(1))"), "CL = THETA(1) * exp(ETA(1));")
  expect_equal(as_("DADT(1) = -KA*A(1)"), "DADT(1) = -KA*A(1);")
  expect_equal(as_("double CL = 0"), "double CL = 0;")
  expect_equal(as_("F1 = 0.5"), "F1 = 0.5;")
})

test_that("convert_semicolons: leaves existing semicolon alone", {
  expect_equal(as_("CL = THETA(1);"), "CL = THETA(1);")
})

test_that("convert_semicolons: converts Fortran inline comment to C++ comment", {
  expect_equal(
    as_("CL = THETA1 ; this is clearance"),
    "CL = THETA1 ; // this is clearance"
  )
  expect_equal(
    as_("V = THETA2 ;volume of distribution"),
    "V = THETA2 ; // volume of distribution"
  )
})

test_that("convert_semicolons: leaves existing C++ comment after semicolon alone", {
  expect_equal(
    as_("CL = THETA(1); // clearance"),
    "CL = THETA(1); // clearance"
  )
})

test_that("convert_semicolons: semicolon between code and C++ comment", {
  expect_equal(as_("a = b // comment"), "a = b; // comment")
})

test_that("convert_semicolons: skips lines ending with continuation operator", {
  expect_equal(as_("CL = THETA(1) *"),   "CL = THETA(1) *")
  expect_equal(as_("CL = THETA(1) +"),   "CL = THETA(1) +")
  expect_equal(as_("CL = THETA(1) -"),   "CL = THETA(1) -")
  expect_equal(as_("CL = THETA(1) /"),   "CL = THETA(1) /")
  expect_equal(as_("CL = THETA(1) &"),   "CL = THETA(1) &")
  expect_equal(as_("CL = THETA(1),"),    "CL = THETA(1),")
  expect_equal(as_("double f("),         "double f(")
})

test_that("convert_semicolons: for loop semicolons inside parens are ignored", {
  expect_equal(
    as_("for(int i = 0; i < n; i++) x += A(i)"),
    "for(int i = 0; i < n; i++) x += A(i);"
  )
  expect_equal(
    as_("for(int i = 0; i < n; i++) {"),
    "for(int i = 0; i < n; i++) {"
  )
})

test_that("convert_semicolons: multiple statements left alone", {
  # second statement contains '=' -> treated as C++, not a comment
  expect_equal(as_("x = 1; y = 2"),  "x = 1; y = 2")
  # second segment already has its own ';'
  expect_equal(as_("x = 1; y = 2;"), "x = 1; y = 2;")
})

test_that("convert_semicolons: skips brace lines", {
  expect_equal(as_("if(WT>=70) {"), "if(WT>=70) {")
  expect_equal(as_("}"),            "}")
  expect_equal(as_("} else {"),     "} else {")
})

test_that("convert_semicolons: handle test condition with no brace", {
  expect_equal(as_("if(WT>=70)"), "if(WT>=70)")
})

test_that("convert_semicolons: skips blank lines", {
  expect_equal(as_(""),   "")
  expect_equal(as_("  "), "  ")
})

test_that("convert_semicolons: skips comment lines", {
  expect_equal(as_("// a comment"),  "// a comment")
  expect_equal(as_("/** comment */"), "/** comment */")
})

test_that("convert_semicolons: adds semicolon to function call ending with )", {
  expect_equal(as_("foo()"),        "foo();")
  expect_equal(as_("bar(x, y)"),    "bar(x, y);")
})

test_that("convert_semicolons: skips preprocessor directives", {
  expect_equal(as_("#define X 1"), "#define X 1")
  expect_equal(as_("#include <cmath>"), "#include <cmath>")
})

test_that("convert_semicolons: strips trailing whitespace before semicolon", {
  expect_equal(as_("V = THETA(2)   "), "V = THETA(2);")
})

test_that("convert_semicolons: works on character vectors", {
  x <- c("CL = THETA(1)", "if(WT>=70) {", "  V = THETA(2)", "}")
  expect_equal(as_(x), c("CL = THETA(1);", "if(WT>=70) {", "  V = THETA(2);", "}"))
})

test_that("convert_semicolons: indentation preserved", {
  expect_equal(as_("  CL = THETA(1)"), "  CL = THETA(1);")
})

test_that("convert_semicolons: skips Fortran block-structure keywords", {
  expect_equal(as_("IF(FLAG.EQ.1) THEN"),       "IF(FLAG.EQ.1) THEN")
  expect_equal(as_("ELSEIF(FLAG.EQ.2) THEN"),   "ELSEIF(FLAG.EQ.2) THEN")
  expect_equal(as_("ELSE"),                      "ELSE")
  expect_equal(as_("ELSE IF(FLAG.EQ.2) THEN"),  "ELSE IF(FLAG.EQ.2) THEN")
  expect_equal(as_("ENDIF"),                     "ENDIF")
  expect_equal(as_("END IF"),                    "END IF")
})

test_that("convert_semicolons: skips block option lines starting with @", {
  expect_equal(as_("@annotated"),        "@annotated")
  expect_equal(as_("@covariates"),       "@covariates")
  expect_equal(as_("@ flag"),            "@ flag")
  expect_equal(as_("  @annotated"),      "  @annotated")
})

test_that("convert_semicolons: non-character input passes through", {
  expect_equal(as_(42),   42)
  expect_null(as_(NULL))
})

test_that("semicolons are not added without nm-vars and semicolons plugins", {
  code <- "$plugin semicolons\n $pk a = 2"
  expect_warning(
    mcode("plugin-1", code, compile = FALSE), 
    "only works with the `nm-vars`", 
    fixed = TRUE
  )
  
  code <- "$plugin semicolons nm-vars\n $pk a = 2"
  mod <- mcode("plugin-2", code, compile = FALSE)
  xx <- readLines(mod@shlib$source)
  xx <- xx[grepl("a =", xx, fixed = TRUE)]
  expect_identical(xx, "a = 2;")
  
  code <- "$plugin nm-like\n $pk a = 2"
  mod <- mcode("plugin-2", code, compile = FALSE)
  xx <- readLines(mod@shlib$source)
  xx <- xx[grepl("a =", xx, fixed = TRUE)]
  expect_identical(xx, "a = 2;")
  
  code <- "$plugin nm-vars\n $pk a = 2"
  mod <- mcode("plugin-2", code, compile = FALSE)
  xx <- readLines(mod@shlib$source)
  xx <- xx[grepl("a =", xx, fixed = TRUE)]
  expect_identical(xx, "a = 2")
})
