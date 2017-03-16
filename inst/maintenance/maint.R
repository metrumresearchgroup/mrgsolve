

satellite_package <- function(all_tests=FALSE,pkgname="mrgsolve") {
  
  tmp <- tempdir()
  chkdir <- file.path(tmp,paste0(pkgname,"_check"))
  if(dir.exists(chkdir)) unlink(chkdir,recursive=TRUE)
  
  foo <- devtools::build('.')
  x <- utils::untar(foo,exdir=chkdir)
  
  if(all_tests) {
    xtests <- list.files(file.path("inst","maintenance", "unit"),
                         full.names=TRUE)  
    file.copy(xtests,
              file.path(chkdir, pkgname, "tests", "testthat"),
              overwrite=TRUE)
  }
  file.path(chkdir,pkgname)
}
