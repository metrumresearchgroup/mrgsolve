
message("\n\nBuilding Documentation \n")

pkg <- file.path(".")

inst <- file.path(pkg,"inst")
inc <- file.path(pkg, "inst", "include")

x1 <- file.copy(
  file.path(inc,"modelheader.h"),
  file.path(inst,"base", "modelheader.h"),
  overwrite=TRUE
)

x2 <- file.copy(
  file.path(inc,"mrgsolv.h"),
  file.path(inst,"base", "mrgsolv.h"),
  overwrite=TRUE
)

stopifnot(all(c(x1,x2)))

roxygen2::roxygenize()

message("\nFinished with roxygenize. \n")

