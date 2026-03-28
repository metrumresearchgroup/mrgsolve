
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(tools))

load_mrgsolve_dll <- function() {
  dll_path <- file.path("src", paste0("mrgsolve", .Platform$dynlib.ext))
  if(!("mrgsolve" %in% names(getLoadedDLLs()))) {
    if(!file.exists(dll_path)) {
      message("Compiling mrgsolve DLL...")
      pkgbuild::compile_dll(quiet = TRUE)
    }
    suppressPackageStartupMessages(library(Rcpp))
    dyn.load(dll_path)
  }
  source("R/RcppExports.R")
}

fun <- function() {
  load_mrgsolve_dll()
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
  source("R/handle_spec_block.R")
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
  source("R/env.R")
  proj <- file.path("inst", "project")
  mod <- mread("housemodel", proj, compile = FALSE, udll=FALSE, ns = FALSE)
  cpp <- normalizePath(list.files(mod@soloc, full.names=TRUE))
  x <- file.copy(cpp, "src", overwrite=TRUE)
  if(!all(x)) stop("Failed to build house model")
}

fun()
rm(list=ls())
