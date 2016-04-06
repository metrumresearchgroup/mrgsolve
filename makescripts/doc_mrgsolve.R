#!/usr/bin/Rscript
#.libPaths("~/Rlibs/lib")

message("\n\nwriting documentation ... \n")

library(methods)
library(devtools)

pkg <- file.path("rdev")

document(pkg)


## message("\nwriting header files for nullmodel and housemodel\n")
r <- file.path(pkg,"R")
src <- file.path(pkg,"src")
inc <- file.path(pkg, "inst", "include")
proj <- file.path(pkg, "inst", "project")


foo <- mread("housemodel", project=proj)

ret <- list()
ret$param <- param(foo)
ret$init <- init(foo)
ret$npar <- length(param(foo))
ret$ncmt <- length(init(foo))
ret$omega <- omat(foo)
 

cpp <- normalizePath(mrgsolve:::compfile("housemodel", proj))

file.copy(cpp, file.path(pkg, "src"),overwrite=TRUE)

saveRDS(file=file.path(pkg,"inst", "project","housemodel.RDS"),foo)







