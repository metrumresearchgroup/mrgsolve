message("\n\nwriting documentation ... \n")

library(methods)
library(devtools)

pkg <- file.path("rdev")
## message("\nwriting header files for nullmodel and housemodel\n")
r <- file.path(pkg,"R")
src <- file.path(pkg,"src")
inst <- file.path(pkg,"inst")
inc <- file.path(pkg, "inst", "include")
proj <- file.path(pkg, "inst", "project")

x1 <- file.copy(file.path(inc,"modelheader.h"),file.path(inst,"base", "modelheader.h"),overwrite=TRUE)
x2 <- file.copy(file.path(inc,"mrgsolv.h"),file.path(inst,"base", "mrgsolv.h"),overwrite=TRUE)
stopifnot(all(c(x1,x2)))


document(pkg)


## I think mrgsolve functions available after doc
foo <- mrgsolve:::as_pack_mod("housemodel",proj, "mrgsolve")
foo$mod <- mrgsolve:::embed_details(foo$mod)




cpp <- normalizePath(foo$source)

x <- file.copy(cpp, file.path(pkg, "src"),overwrite=TRUE)

mod <- foo$mod
saveRDS(file=file.path(pkg,"inst", "project","housemodel.RDS"),foo$mod)





