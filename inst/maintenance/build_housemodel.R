
fun <- expression( {
  .libPaths("~/Rlibs")
  library(dplyr)
  setClass("mrgsims")
  source("R/utils.R")
  setClass("mrgsims")
  source("R/generics.R")
  source("R/class_build.R")
  source("R/class_ev.R")
  source("R/class_matlist.R")
  source("R/class_numericlist.R")
  source("R/class_derived.R")
  source("R/class_mrgmod.R")

  source("R/mread.R")
  source("R/modspec.R")
  source("R/compile.R")
  source("R/Aaaa.R")
  source("R/annot.R")
  source("R/matrix.R")
  source("R/matlist.R")
  source("R/relabel.R")
  source("R/model_include.R")
  source("R/param.R")
  source("R/init.R")
  source("R/funset.R")
  source("R/update.R")
  
  proj <- file.path("inst", "project")
  foo <- as_pack_mod("housemodel", proj, "mrgsolve")
  cpp <- normalizePath(foo$source)
  pkg <- file.path('.')
  x <- file.copy(cpp, file.path(pkg, "src"),overwrite=TRUE)
  saveRDS(file=file.path(pkg,"inst", "project","housemodel.RDS"),foo$mod)
})

message("\nBuilding the mrgsolve::house model.\n")
en <- new.env()
source(exprs = fun, local = en)



