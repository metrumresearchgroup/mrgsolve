library(covr)

options(covr.gcov = "")
options(covr.flags=c(CFLAGS=""))

src <- "."

tgt <- file.path(tempdir(),"covr")

if(dir.exists(tgt)) unlink(tgt, recursive = TRUE)

dir.create(tgt)

file.copy(src, tgt, recursive = TRUE)

addl <- list.files(file.path(src, "inst", "maintenance", "unit"), full.names=TRUE)

to <- file.path(tgt,  "tests", "testthat", basename(addl))

file.copy(addl, to, overwrite = TRUE)

ln <- c("R/print.R", "R/package.R", "R/qsim.R", "R/render.R")

x <- package_coverage(file.path(tgt), quiet=FALSE, line_exclusions=ln)

y <- coverage_to_list(x)

z <- zero_coverage(x)
write.csv(z, file = "inst/maintenance/unit/zero.md")


df <- data.frame(file = names(y$filecoverage), coverage = y$filecoverage, row.names=NULL)
df <- df[order(as.numeric(df$coverage)),]

outfile <- "inst/maintenance/unit/coverage.md"
cat(file=outfile, "# coverage: ",y$totalcoverage, "%\n\n", sep="")
cat(file=outfile, append = TRUE, knitr::kable(df,row.names=FALSE),sep="\n")

x
