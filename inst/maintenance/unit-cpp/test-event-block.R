library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")

options("mrgsolve_mread_quiet"=TRUE)

local_edition(3)

code_event <- '
$PLUGIN evtools nm-vars autodec

$PARAM 
Ii    =  24
Ss    =  1
Addl  =  3
Amt   =  100
Rate  =  20
CL    = 2
V     = 20

$PKMODEL cmt = "B"

$EVENT
if(TIME==0) {
  evt::ev dose = evt::infuse(Amt, 1, Rate); 
  evt::ss(dose, Ss); 
  evt::ii(dose, Ii); 
  evt::addl(dose, Addl); 
  self.push(dose);
}
capture b = A(1)/V;
capture c = 1.23;
double d = 50;

$ERROR
capture cp = B/V;
e = d/2;

$CAPTURE e d
'

code_error <- '
$PLUGIN evtools nm-vars autodec

$PARAM 
Ii    =  24
Ss    =  1
Addl  =  3
Amt   =  100
Rate  =  20
CL    = 2
V     = 20

$PKMODEL cmt = "B"

$ERROR

if(TIME==0) {
  evt::ev dose = evt::infuse(Amt, 1, Rate); 
  evt::ss(dose, Ss); 
  evt::ii(dose, Ii); 
  evt::addl(dose, Addl); 
  self.push(dose);
}
capture cp = B/V;
'

mod1 <- mcode("event", code_event, delta = 0.1, end = 120)
mod2 <- mcode("error", code_error, delta = 0.1, end = 120)

outev <- mrgsim(mod1)
outer <- mrgsim(mod2)

test_that("$EVENT result matches $TABLE results", {
  expect_identical(outev$B, outer$B)
  # Known that cp isn't calculated when $TABLE is used
  expect_equal(outer$cp[1], 0)
  # Using $EVENT allows this to be calculated
  expect_equal(outev$cp[1], outev$B[1]/mod1$V)
  i <- seq(nrow(outer))[-1]
  expect_equal(outer$cp[i], outev$cp[i])
})

test_that("declare inside $EVENT", {
  expect_true(all(outev$d == 50))  
  expect_true(all(outev$e == outev$d/2))
})

test_that("capture from $EVENT", {
  expect_true(all(abs(outev$c - 1.23) < 1e-7)) 
  expect_equal(outev$b, outer$cp)
})

test_that("check internals", {
  f <- mrgsolve:::funset(mod1)
  expect_equal(nrow(f$symbols), 5)
  expect_true(all(f$symbols$loaded))
  expect_equal(f$symbols$name[4], "_model_event_event__")
  
  p <- mrgsolve:::pointers(mod1)
  expect_equal(length(p), 5)
  expect_equal(names(p)[4], "event")
  
  expect_true(mod1@shlib$call_event)
  expect_false(mod2@shlib$call_event)
  
  df <- as.list(mod1)$cpp_variables
  cap <- df[df$type=="capture",] 
  expect_equal(cap$var, c("cp", "b", "c"))
  expect_equal(cap$context, c("table", "event", "event"))
  
  auto <- df[df$context=="auto", ]
  expect_true(all(auto$type=="double"))
  expect_equal(auto$var[1], "e")

  f <- mrgsolve:::funset(house())
  expect_equal(nrow(f$symbols), 5)
  expect_true(all(f$symbols$loaded))
  expect_equal(f$symbols$name[4], "_model_housemodel_event__")
  
  p <- mrgsolve:::pointers(house())
  expect_equal(length(p), 5)
  expect_equal(names(p)[4], "event")
  
  expect_false(house()@shlib$call_event)
})


