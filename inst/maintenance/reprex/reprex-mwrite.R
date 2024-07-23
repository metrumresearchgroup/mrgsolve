library(mrgsolve)
library(dplyr)

mod <- modlib("popex")

#' Write to yaml
a <- mwrite_yaml(mod, "popex-2.yaml")

yam <- readLines("popex-2.yaml")

#' omega rendered like this
cat(yam[21:37], sep = "\n")

#' Now, write to native mrgsolve format
x <- mwrite_cpp(mod, "popex-2.mod")

mod2 <- mread(x$file)

cat(mod2@code[14:26], sep = "\n")

#' Simulate from this 
mrgsim(mod2, ev(amt = 100, ID = 1:10)) %>% plot()

#' Convert a matrix to something for writing out
omat(mod)

mrgsolve:::get_upper_tri(omat(mod))

