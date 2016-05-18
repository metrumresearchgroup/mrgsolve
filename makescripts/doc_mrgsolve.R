message("\n\nwriting documentation ... \n")

library(methods)
library(devtools)

pkg <- file.path("rdev")

document(pkg)

cppfile <- function(x) {
  file.path(proj,paste0(x@model, ".cpp.cpp"))
}

## message("\nwriting header files for nullmodel and housemodel\n")
r <- file.path(pkg,"R")
src <- file.path(pkg,"src")
inc <- file.path(pkg, "inst", "include")
proj <- file.path(pkg, "inst", "project")

## I think mrgsolve functions available after doc
foo <- mrgsolve:::as_pack_mod("housemodel",proj, "mrgsolve")

cpp <- normalizePath(cppfile(foo))

x <- file.copy(cpp, file.path(pkg, "src"),overwrite=TRUE)

saveRDS(file=file.path(pkg,"inst", "project","housemodel.RDS"),foo)

