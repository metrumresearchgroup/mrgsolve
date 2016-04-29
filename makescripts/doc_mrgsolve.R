message("\n\nwriting documentation ... \n")

library(methods)
library(devtools)

pkg <- file.path("rdev")

document(pkg)

cppfile <- function(x) {
  file.path(proj,paste0(x@package, ".cpp.cpp"))
}

## message("\nwriting header files for nullmodel and housemodel\n")
r <- file.path(pkg,"R")
src <- file.path(pkg,"src")
inc <- file.path(pkg, "inst", "include")
proj <- file.path(pkg, "inst", "project")

## I think mrgsolve functions available after doc
foo <- mread("housemodel", project=proj, compile=FALSE,udll=FALSE)
foo <- compiled(foo,TRUE)
foo@shlib$par <- pars(foo)
foo@shlib$cmt <- cmt(foo)

ret <- list()
ret$param <- param(foo)
ret$init <- init(foo)
ret$npar <- length(param(foo))
ret$ncmt <- length(init(foo))
ret$omega <- omat(foo)

cpp <- normalizePath(cppfile(foo))

file.copy(cpp, file.path(pkg, "src"),overwrite=TRUE)

saveRDS(file=file.path(pkg,"inst", "project","housemodel.RDS"),foo)

# x <- getNamespace("mrgsolve")
# 
# for(model in c("modpk1", "modpk1po", "modpk2", "modpk2po")) {
#   writeLines( x[[model]],file.path(proj,paste0(model, ".cpp")))
#   foo <- mread(model,proj,compile=FALSE,udll=FALSE)
#   foo <- compiled(foo,TRUE)
#   foo@shlib$par <- pars(foo)
#   foo@shlib$cmt <- cmt(foo)
#   cpp <- normalizePath(cppfile(foo))
#   file.copy(cpp,file.path(pkg,"src"), overwrite=TRUE)
#   saveRDS(file=file.path(pkg, "inst", "project", paste0(model, ".RDS")),foo)
# }
# 









