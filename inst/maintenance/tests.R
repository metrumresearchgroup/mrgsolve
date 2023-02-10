
library(mrgsolve)
library(testthat)
library(dplyr)

message("tests/testthat")
a <- test_dir("tests/testthat")
a$result <- NULL
message("\ninst/maintenance/unit")
b <- test_dir("inst/maintenance/unit/")
b$result <- NULL
results <- dplyr::bind_rows(as_tibble(a),as_tibble(b))
results$user <- NULL
results$system <- NULL
results$real <- NULL
results$result <- NULL
tab <- knitr::kable(results, format="markdown")
cat(file="inst/maintenance/unit/tests.md", tab, sep="\n")
