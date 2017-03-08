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

context("test-modspec")

options(mrgsolve_mread_quiet=TRUE)

mtemp <- function(...) {
  
  mcode(model=basename(tempfile()),..., compile=FALSE)
}
test_that("Parse matrix", {
  
  code <- "$OMEGA \n 1 2 \n 3"
  mod <- mtemp(code)
  expect_equal(dim(omat(mod))[[1]],c(3,3))
  
  code <- "$OMEGA \n @block \n 1 0.002 \n 3"
  mod <- mtemp(code)
  expect_equal(dim(omat(mod))[[1]],c(2,2))  
  
})



test_that("Parse capture", {
  
  code <- "$CAPTURE\n yes=TRUE \n z a"
  mod <- mtemp(code)
  expect_equal(mod@capture, c("z", "a"))
  
  code <- "$CAPTURE\n yes=TRUE \n z a \n\n\n d\n e, f"
  mod <- mtemp(code)
  expect_equal(mod@capture, c("z", "a", "d", "e", "f"))
  
  code <- "$CAPTURE >> yes=TRUE \n z a \n\n\n d\n e, f"
  mod <- mtemp(code)
  expect_equal(mod@capture, c("z", "a", "d", "e", "f"))
  
  code <- "$CAPTURE \n"
  expect_warning(mod <- mtemp(code))
  expect_equal(mod@capture, character(0))
  
})


test_that("Parse cmt", {
  
  code <- "$CMT\n yes=TRUE \n first \n \n \n second third \n \n"
  mod <- mtemp(code)
  expect_equal(mrgsolve:::cmt(mod), c("first", "second", "third"))

})


test_that("Parse theta", {
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
    KA // Capturing KA
  ' 
  
  expect_is(mod <- mcode("commented", code),"mrgmod")
  expect_identical(param(mod),param(CL=2,VC=10,KA=3))
  expect_identical(init(mod),init(x=0,y=3,h=3))
  expect_identical(mod@capture, c("KA","a"))
  
})


test_that("Parse at options", {
  
  ats <- mrgsolve:::parse_ats
  
  code <- '
  
  @bool1
  @bool2

  @name some person
  @  zip   55455 @town minneapolis @city
  @ state mn @midwest @x 2
  '
  
  x <- unlist(strsplit(code, "\n"))
  x <- ats(x)
  
  expect_is(x,"list")
  expect_identical(x$bool1,TRUE)
  expect_identical(x$bool2,TRUE)
  expect_identical(x$city,TRUE)
  expect_identical(x$midwest,TRUE)
  expect_identical(x$name,"some person")
  expect_identical(x$state,"mn")  
  expect_identical(x$town,"minneapolis")
  expect_equal(x$x,2)
  expect_warning(ats(" @ ' a b c'"))  
  expect_warning(ats('@ "a b c"'))  
  
})




