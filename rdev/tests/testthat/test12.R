library(mrgsolve)
library(testthat)
require(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)
project <- file.path(system.file(package="mrgsolve"), "models")

context("Data set record sorting")

mod <- mrgsolve:::house()

data(exTheoph)

dd1 <- exTheoph %>% bind_rows(., group_by(.,ID) %>% slice(5)) %>% arrange(ID) 
dd2 <- dd1 %>% arrange(ID,time)


test_that("Improperly sorted records produces error", {
    expect_error(mod %>% data_set(dd1) %>% mrgsim)
})

test_that("Properly sorted records produces no error", {
  expect_is((mod %>% data_set(dd2) %>% mrgsim),"mrgsims")
})


