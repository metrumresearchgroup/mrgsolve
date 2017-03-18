source(file.path("inst", "maintenance","maint.R"))
pkg <- satellite_package(all_tests=TRUE)
keepdir <- file.path("Rchecks", "gut_check")
if(!dir.exists(keepdir)) dir.create(keepdir)
devtools::check(basename(pkg),check_dir=keepdir)




