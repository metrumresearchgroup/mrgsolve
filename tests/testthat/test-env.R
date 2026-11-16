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

context("test-env")

test_that("ENV", {
  mod <- mcode("test_env_1", "$ENV a <- 1\nb <- 2",compile=FALSE)
  expect_is(mod, "mrgmod")
})  

test_that("ENV sub into $PARAM", {
  code <- '
  $ENV a <- 1; b <- 2
  $PARAM A = a, B = b
  '
  mod <- mcode("test_env_2", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  expect_identical(mrgsolve:::pars(mod),c("A", "B"))
  expect_equivalent(as.numeric(param(mod)), c(1,2))
})

test_that("ENV sub into $INIT", {
  code <- '
  $ENV a <- 1\n b <- 2
  $INIT A = a, B = b
  '
  mod <- mcode("test_env_3", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  expect_identical(mrgsolve:::cmt(mod),c("A", "B"))
  expect_equivalent(as.numeric(init(mod)), c(1,2))
})

test_that("Get $OMEGA matrix from $ENV", {
  code <- '
  $ENV mat <- dmat(11,22,33)
  $OMEGA  @object mat @labels z y x @name OM
  '
  mod <- mcode("test-env-5", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  m <- as.matrix(omat(mod))
  expect_identical(names(omat(mod)), "OM")
  expect_identical(diag(m),c(11,22,33))
  expect_identical(labels(omat(mod))[[1]], c("z", "y", "x"))
  
})


test_that("ENV sub into $FIXED", {
  code <- '
  $ENV a <- 1\n b <- 2
  $FIXED A = a, B = b
  '
  mod <- mcode("test-env-6", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  x <- allparam(mod)
  expect_identical(names(x),c("A", "B"))
  expect_equivalent(as.numeric(x), c(1,2))
  
  l <- env_ls(mod)
  expect_is(l,"data.frame")
})


code <- '
$ENV
x <- 1
y <- 2
e <- ev(amt=100)
df <- tibble(ID=1:2,amt=3)
'


test_that("env-funs", {
  mod <- mcode("env-funs", code, compile = FALSE)

  expect_is(env_ls(mod),"data.frame")

  mm <- env_update(mod,y=3)
  expect_equal(env_get(mm, "y"), 3)

  mmm <- env_eval(mod)
  expect_is(mmm,"mrgmod")
})

test_that("env_get.mrgmod retrieves object by name", {
  mod <- house(end = 1)
  assign("let", letters, mod@envir)
  result <- env_get(mod, "let")
  expect_identical(result, letters)
})


test_that("env_get.mrgsims retrieves same object as env_get.mrgmod", {
  mod <- house(end = 1)
  assign("let", letters, mod@envir)
  out <- mrgsim(mod)
  expect_identical(env_get(out, "let"), env_get(mod, "let"))
})

test_that("env_get_env.mrgmod returns an environment", {
  mod <- house(end = 1)
  e <- env_get_env(mod)
  expect_true(is.environment(e))
})

test_that("env_get_env.mrgsims returns an environment", {
  mod <- house(end = 1)
  out <- mrgsim(mod)
  e <- env_get_env(out)
  expect_true(is.environment(e))
})

test_that("env_get_obj is an alias to env_get", {
  mod <- house(end = 1)
  assign("lett", letters, mod@envir)
  out <- mrgsim(mod)
  a <- env_get(out, "lett")
  b <- env_get_obj(out, "lett")
  expect_identical(a,b)
  c <- env_get(mod, "lett")
  d <- env_get_obj(mod, "lett")
  expect_identical(c,d)
})

test_that("env_get_env returns the same environment from mod and out", {
  mod <- house(end = 1)
  out <- mrgsim(mod)
  expect_identical(env_get_env(mod), env_get_env(out))
})

test_that("objects assigned via env_get_env are visible via env_get from mrgsims", {
  mod <- house(end = 1)
  assign("let", letters, env_get_env(mod))
  out <- mrgsim(mod)
  expect_identical(env_get(out, "let"), letters)
})

test_that("env_get requires what argument for mrgmod", {
  mod <- house(end = 1)
  expect_error(env_get(mod), "missing, with no default")
  expect_error(env_get(mod, letters[1:2]), "must be character with length 1")
  expect_error(env_get(mod, FALSE), "must be character with length 1")
})

test_that("env_get errors on missing object", {
  mod <- house(end = 1)
  expect_error(env_get(mod, "noex"), "object 'noex' not found")
})

# $ENV parent environment ----

env_parent <- function(code, ...) {
  mod <- mcode("env-parent", code, compile = FALSE, ...)
  parent.env(mod@envir)
}

test_that("no $ENV block gives a base environment", {
  mod <- mcode("env-none", "$PARAM a = 1", compile = FALSE)
  expect_s4_class(mod, "mrgmod")
  expect_identical(parent.env(mod@envir), baseenv())
  expect_null(mod@envir$.code)
  e <- mrgsolve:::handle_ENV(NULL, tempdir())
  expect_identical(parent.env(e), baseenv())
  expect_length(ls(e, all.names = TRUE), 0)
})

test_that("$ENV block defaults to the mrgsolve namespace", {
  ns <- asNamespace("mrgsolve")
  expect_identical(env_parent("$ENV a <- 1"), ns)
  expect_identical(env_parent("$ENV\n@parent mrgsolve\na <- 1"), ns)
  # internal functions are visible
  mod <- mcode("env-ns", "$ENV f <- eval_ENV_block", compile = FALSE)
  expect_true(is.function(env_get(mod, "f")))
  # unprefixed non-base functions resolve via the search path
  mod <- mcode("env-ns2", "$ENV r <- length(rnorm(3))", compile = FALSE)
  expect_equal(env_get(mod, "r"), 3)
})

test_that("default parent does not expose eval_ENV_block locals", {
  e <- mrgsolve:::handle_ENV("a <- 1", tempdir())
  expect_identical(parent.env(e), asNamespace("mrgsolve"))
  expect_false(exists("parse_status", envir = e))
  expect_false(exists("noenv", envir = e))
  expect_false(exists("cwd", envir = e))
  expect_identical(sort(ls(e, all.names = TRUE)), c(".code", "a"))
})

test_that("@parent base gives base-only environment", {
  code <- "$ENV\n@parent base\na <- 1"
  expect_identical(env_parent(code), baseenv())
  expect_error(
    mcode("env-base-1", "$ENV\n@parent base\nr <- rnorm(1)", 
          compile = FALSE),
    "Failed to parse code in \\$ENV"
  )
  expect_error(
    mcode("env-base-2", "$ENV\n@parent base\nh <- head(1:3)", 
          compile = FALSE),
    "Failed to parse code in \\$ENV"
  )
  # package prefix still works
  mod <- mcode(
    "env-base-3", 
    "$ENV\n@parent base\nr <- length(stats::rnorm(4))", 
    compile = FALSE
  )
  expect_equal(env_get(mod, "r"), 4)
})

test_that("invalid or quoted parent option errors", {
  expect_error(
    mcode("env-bad-1", "$ENV\n@parent bogus\na <- 1", compile = FALSE),
    "should be one of"
  )
  expect_error(
    suppressWarnings(
      mcode("env-bad-2", "$ENV\n@parent \"base\"\na <- 1", compile = FALSE)
    ),
    "should be one of"
  )
})

test_that("user cannot pass envir via block option", {
  e <- mrgsolve:::handle_ENV("@envir 1\na <- 1", tempdir())
  expect_true(is.environment(e))
  expect_equal(e$a, 1)
})

test_that("block options are removed from stored code", {
  mod <- mcode(
    "env-code", "$ENV\n@parent base\na <- 1", compile = FALSE
  )
  expect_identical(mod@envir$.code, "a <- 1")
})

test_that("block with only options is allowed", {
  mod <- mcode("env-onlyopt", "$ENV @parent base", compile = FALSE)
  expect_s4_class(mod, "mrgmod")
  expect_identical(parent.env(mod@envir), baseenv())
  expect_no_error(env_eval(mod))
})

test_that("parent option is respected when objects feed other blocks", {
  for(parent in c("mrgsolve", "base")) {
    code <- paste0(
      "$ENV\n@parent ", parent, "\na <- 2\nb <- c(a, 3)\n",
      "$PARAM A = a, B = b[2]\n"
    )
    mod <- mcode(paste0("env-flow-", parent), code, compile = FALSE)
    expect_equivalent(as.numeric(param(mod)), c(2, 3), info = parent)
  }
})

test_that("env_eval re-evaluates in the existing environment", {
  mod <- mcode(
    "env-reeval", 
    "$ENV\n@parent base\nn <- 1", 
    compile = FALSE
  )
  env <- mod@envir
  assign("n", 99, envir = env)
  env_eval(mod)
  expect_identical(mod@envir, env)
  expect_equal(env_get(mod, "n"), 1)
  expect_identical(parent.env(mod@envir), baseenv())
})

test_that("supplied envir is not re-parented", {
  e <- new.env(parent = globalenv())
  res <- mrgsolve:::eval_ENV_block("a <- 1", tempdir(), envir = e, parent = "base")
  expect_identical(res, e)
  expect_identical(parent.env(e), globalenv())
  
  e <- new.env()
  expect_identical(mrgsolve:::eval_ENV_block(NULL, tempdir(), envir = e), e)
})

test_that("$ENV code is evaluated in the project directory", {
  dir <- tempfile("env-where-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  cwd <- getwd()
  e <- mrgsolve:::handle_ENV("wd <- getwd()", dir)
  expect_identical(normalizePath(e$wd), normalizePath(dir))
  expect_identical(getwd(), cwd)
})

test_that("working directory is restored when $ENV code fails", {
  cwd <- getwd()
  expect_error(
    mrgsolve:::handle_ENV("stop('fail')", tempdir()), 
    "Failed to parse code in \\$ENV"
  )
  expect_error(
    mrgsolve:::handle_ENV("a <- ", tempdir()), 
    "Failed to parse code in \\$ENV"
  )
  expect_identical(getwd(), cwd)
})

test_that("block options evaluate in a base environment when no $ENV", {
  e <- mrgsolve:::handle_ENV(NULL, tempdir())
  opts <- mrgsolve:::scrape_opts(">> a = stats::median(c(1, 5, 9))", envir = e)
  expect_equal(opts$a, 5)
  expect_error(
    mrgsolve:::scrape_opts(">> a = median(c(1, 5, 9))", envir = e),
    "median"
  )
})
