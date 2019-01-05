
message("\n\nBuilding Documentation \n")

# pkg <- file.path(".")
# 
# inst <- file.path(pkg,"inst")
# inc <- file.path(pkg, "inst", "include")
# 
# x1 <- file.copy(
#   file.path(inc,"modelheader.h"),
#   file.path(inst,"base", "modelheader.h"),
#   overwrite=TRUE
# )
# 
# x2 <- file.copy(
#   file.path(inc,"mrgsolv.h"),
#   file.path(inst,"base", "mrgsolv.h"),
#   overwrite=TRUE
# )
# 
# stopifnot(all(c(x1,x2)))

roxygen2::roxygenize()

# ## I think mrgsolve functions available after doc
# proj <- file.path("inst", "project")
# source("R/modspec.R")
# foo <- mrgsolve:::as_pack_mod("housemodel", proj, "mrgsolve")
# mod <- foo$mod
# saveRDS(file=file.path(pkg,"inst", "project","housemodel.RDS"),foo$mod,version=2)

message("\nFinished with roxygenize. \n")

