library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

test_that("Switch to custom tolerances", {
  modx <- house()
  n <- mrgsolve:::neq(modx)
  expect_length(modx@vec_rtol, 0)
  expect_length(modx@vec_rtol, 0)
  mod <- use_custom_tol(modx)
  expect_length(mod@vec_rtol, n)
  expect_length(mod@vec_rtol, n)
  expect_equal(mod@itol, 4)
  
  expect_error(
    use_custom_tol(letters), 
    "the first argument to use_custom_tol"
  )
})

test_that("Switch to scalar tolerances", {
  modx <- use_custom_tol(house())
  n <- mrgsolve:::neq(modx)
  expect_equal(modx@itol, 4)
  mod <- use_scalar_tol(modx)
  expect_equal(mod@itol, 1)
  expect_length(mod@vec_rtol, n)
  expect_length(mod@vec_rtol, n)
  
  expect_error(
    use_scalar_tol(letters), 
    "the first argument to use_scalar_tol"
  )
})

test_that("Extract tolerances", {
  mod <- house()
  
  # data frame
  data <- custom_tol(mod)
  expect_equal(ncol(data), 5)
  
  # prior to initializing
  expect_true(all(is.na(data$custom_rtol)))
  expect_true(all(is.na(data$custom_atol)))
  
  # after initializing
  data <- custom_tol(use_custom_tol(mod))
  expect_true(all(data$custom_rtol==mod@rtol))
  expect_true(all(data$custom_atol==mod@atol))
  
  # list
  data2 <- custom_tol_list(use_custom_tol(mod))
  expect_is(data2, "list")
  expect_equal(names(data2), c("custom_rtol", "custom_atol", "scalar_rtol", "scalar_atol"))
  expect_equal(as.list(data$custom_rtol), unname(data2$custom_rtol))
  expect_equal(as.list(data$custom_atol), unname(data2$custom_atol))
  
  # names
  expect_identical(mrgsolve:::Cmt(mod), names(data2$custom_rtol))
  expect_identical(mrgsolve:::Cmt(mod), names(data2$custom_atol))
  expect_identical(mrgsolve:::Cmt(mod), names(data2$scalar_rtol))
  expect_identical(mrgsolve:::Cmt(mod), names(data2$scalar_rtol))
  
  expect_error(
    custom_tol(letters), 
    "the first argument to custom_tol"
  )
  expect_error(
    custom_tol_list(letters), 
    "the first argument to custom_tol_list"
  )
})

test_that("Customize tolerances - dots", {
  mod <- use_custom_tol(house(rtol = 1e-4, atol = 1e-5))
  data <- custom_tol_list(mod)
  expect_equal(data$custom_rtol, list(GUT = 1e-4, CENT = 1e-4, RESP = 1e-4))
  expect_equal(data$custom_atol, list(GUT = 1e-5, CENT = 1e-5, RESP = 1e-5))
  
  mod <- customize_rtol(mod, CENT = 1e-3, RESP = 1e-9)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_rtol, list(GUT = 1e-4, CENT = 1e-3, RESP = 1e-9))
  mod <- customize_atol(mod, GUT = 1e-10, RESP = 1e-12)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_atol, list(GUT = 1e-10, CENT = 1e-5, RESP = 1e-12))
  
  expect_error(customize_rtol(mod, kyle = 10), "Invalid compartment names")
  expect_error(customize_atol(mod, metrum = 10), "Invalid compartment names")
  
  expect_error(
    customize_rtol(letters), 
    "the first argument to customize_rtol"
  )
  expect_error(
    customize_atol(letters), 
    "the first argument to customize_atol"
  )
})


test_that("Customize tolerances - object", {
  mod <- use_custom_tol(house(rtol = 1e-4, atol = 1e-5))
  data <- custom_tol_list(mod)

  up <- c(CENT = 1e-3, RESP = 1e-9)
  mod <- customize_rtol(mod, up)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_rtol, list(GUT = 1e-4, CENT = 1e-3, RESP = 1e-9))
  
  up <- c(GUT = 1e-10, RESP = 1e-12)
  mod <- customize_atol(mod, up)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_atol, list(GUT = 1e-10, CENT = 1e-5, RESP = 1e-12))

  up <- list(GUT = 1e-2, RESP = 1e-3)
  mod <- customize_atol(mod, up)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_atol, list(GUT = 1e-2, CENT = 1e-5, RESP = 1e-3))
    
  up <- list(GUT = 1e-3, RESP = 1e-9)
  mod <- customize_atol(mod, up)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_atol, list(GUT = 1e-3, CENT = 1e-5, RESP = 1e-9))
  
  up <- c(GUT = 1e-2, kyle = 10)
  expect_error(customize_rtol(mod, up), "Invalid compartment names")
  expect_error(customize_atol(mod, up), "Invalid compartment names")
})

test_that("Customize tolerances - both", {
  mod <- use_custom_tol(house(rtol = 1e-12, atol = 1e-13))
  data <- custom_tol_list(mod)
  
  up <- c(GUT = 1e-3, RESP = 1e-9)
  mod <- customize_rtol(mod, up, CENT = 1e-1)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_rtol, list(GUT = 1e-3, CENT = 1e-1, RESP = 1e-9))
  
  up <- c(GUT = 1e-2, RESP = 1e-7)
  mod <- customize_atol(mod, up, CENT = 1e-3)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_atol, list(GUT = 1e-2, CENT = 1e-3, RESP = 1e-7))
  
  up <- c(GUT = 1, CENT = 2, RESP = 3)
  mod <- customize_rtol(mod, up, RESP = 4, GUT = 5, CENT = 6)
  data <- custom_tol_list(mod)
  expect_equal(data$custom_rtol, list(GUT = 5, CENT = 6, RESP = 4))

})

test_that("Reset tolerances", {
  mod <- customize_rtol(house(), .default = 1e-2)   
  n <- mrgsolve:::neq(mod)
  
  data <- custom_tol(mod)
  expect_equal(data$custom_rtol, rep(1e-2, n))
  
  # Just reset rtol
  mod <- reset_rtol(mod, rtol = 1e-7)
  expect_equal(mod@rtol, 1e-7)
  data <- custom_tol(mod)
  expect_equal(data$custom_rtol, rep(1e-7, n))
  
  # Just reset atol
  mod <- reset_atol(mod, atol = 1e-9)
  expect_equal(mod@atol, 1e-9)
  data <- custom_tol(mod)
  expect_equal(data$custom_atol, rep(1e-9, n))
  
  # Reset both rtol and atol
  mod <- reset_tolerances(mod, rtol = 1e-2, atol = 1e-6)
  
  expect_equal(mod@rtol, 1e-2)
  data <- custom_tol(mod)
  expect_equal(data$custom_rtol, rep(1e-2, n))
  
  expect_equal(mod@atol, 1e-6)
  data <- custom_tol(mod)
  expect_equal(data$custom_atol, rep(1e-6, n))
  
  # itol is preserved
  mod <- house()
  expect_equal(mod@itol, 1)
  mod <- reset_tolerances(mod, rtol = 1e-5, atol = 1e-8)
  expect_equal(mod@itol, 1)
  
  mod <- house(rtol = 1, atol = 2)
  mod <- customize_rtol(mod, .default = 3)
  mod <- customize_atol(mod, .default = 4)
  mod <- use_scalar_tol(mod)
  mod <- reset_tolerances(mod)
  expect_equal(mod@itol, 1)
  expect_equal(mod@rtol, 1)
  expect_equal(mod@atol, 2)
  
  mod <- house(rtol = 1)
  mod <- customize_rtol(mod, .default = 3)
  mod <- use_scalar_tol(mod)
  mod <- reset_rtol(mod)
  expect_equal(mod@itol, 1)
  expect_equal(mod@rtol, 1)
  expect_equal(mod@atol, 1e-8, tolerance=1e-5)

  mod <- house(atol = 5)
  mod <- customize_atol(mod, .default = 5)
  mod <- use_scalar_tol(mod)
  mod <- reset_atol(mod)
  expect_equal(mod@itol, 1)
  expect_equal(mod@rtol, 1e-8, tolerance=1e-5)
  expect_equal(mod@atol, 5)
  
  expect_error(
    reset_tolerances(letters), 
    "the first argument to reset_tolerances"
  )
  expect_error(
    reset_tolerances(letters), 
    "the first argument to reset_tolerances"
  )
  expect_error(
    reset_rtol(letters), 
    "the first argument to reset_rtol"
  )
  expect_error(
    reset_atol(letters), 
    "the first argument to reset_atol"
  )
})

test_that("Detect corrupted vec_rtol and vec_atol data", {
  h <- house()
  
  mod <- use_custom_tol(h)
  mod@vec_rtol <- mod@vec_rtol[1:2]
  expect_error(custom_tol(mod), "custom rtol vector is not the correct size")
  
  mod <- use_custom_tol(h)
  set.seed(1023)
  names(mod@vec_rtol) <- sample(mod@vec_rtol)
  expect_error(custom_tol(mod), "custom rtol vector is in the wrong order")
  
  mod <- use_custom_tol(h)
  mod@vec_atol <- mod@vec_atol[1:2]
  expect_error(custom_tol(mod), "custom atol vector is not the correct size")
  
  mod <- use_custom_tol(h)
  set.seed(1023)
  names(mod@vec_atol) <- sample(mod@vec_atol)
  expect_error(custom_tol(mod), "custom atol vector is in the wrong order")
})

test_that("Bad customize input", {
  mod <- use_custom_tol(house())
  expect_error(
    customize_rtol(mod, CENT = "ABC"), 
    "tolerance data must be a named numeric list or vector"
  )
  expect_error(
    customize_atol(mod, atol = c(1,2,3)), 
    "tolerance data must be a named numeric list or vector"
  )
  expect_error(
    customize_rtol(mod, mtcars), 
    "tolerance data must be a named numeric list or vector"
  )
})
