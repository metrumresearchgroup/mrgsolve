
library(mrgsolve)
library(testthat)
library(dplyr)

message("tests/testthat")
e <- new.env()
a <- test_dir("tests/testthat", env = e)
a$result <- NULL
message("\ninst/maintenance/unit")
e <- new.env()
b <- test_dir("inst/maintenance/unit/", env = e)
b$result <- NULL
e <- new.env()
c <- test_dir("inst/maintenance/unit-cpp/", env = e)
c$result <- NULL

results <- dplyr::bind_rows(as_tibble(a),as_tibble(b),as_tibble(c))
results$user <- NULL
results$system <- NULL
results$real <- NULL
results$result <- NULL
tab <- knitr::kable(results, format="markdown")
cat(file="inst/maintenance/unit/tests.md", tab, sep="\n")
