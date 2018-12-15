library(covr)

options(covr.gcov = "")
options(covr.flags=c(CFLAGS=""))

src <- "."

tgt <- file.path(tempdir(),"covr")

if(dir.exists(tgt)) unlink(tgt, recursive = TRUE)

dir.create(tgt)

file.copy(src, tgt, recursive = TRUE)

addl <- list.files(file.path(src, "inst", "maintenance", "unit"), full.names=TRUE)

to <- file.path(tgt,  "tests", "testthat", basename(addl))

file.copy(addl, to, overwrite = TRUE)

ln <- c("R/print.R", "R/package.R", "R/qsim.R", "R/render.R")

package_coverage(file.path(tgt), quiet=FALSE, line_exclusions=ln)



