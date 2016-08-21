library(dplyr)
parse_yaml_line <- function(x,split="\\s*\\@\\s*",...) {
  strsplit(x,split)
}
parse_yaml <- function(x) {
  stopifnot(requireNamespace("yaml"))
  yaml::yaml.load(x)
}
pick2list_yaml <- function(x,what) {
  y <- sapply(x,"[", what)
  y <- paste0("list(",paste(y, collapse=","),")")
  y <- eval(parse(text=y))
  setNames(y,names(x))
}
split3_yaml <- function(x,nm) {
  ## x is a list
  if(is.null(x)) return(NULL)
  nm <- names(x)
  x <- sapply(x,parse_yaml_line)
  v <- pick2list_yaml(x,1L)
  o <- lapply(x, "[", c(2L,3L))
  list(v=v,o=o, nm=nm)
}
split2_yaml <- function(x,nm) {
  ## x is a list
  if(is.null(x)) return(NULL)
  nm <- names(x)
  x <- sapply(x,parse_yaml_line)
  o <- lapply(x, "[", c(1L,2L))
  list(o=o,nm=nm)
}



x <- '
param:
  CL: 2@Clearance@L/hr
  VC: 3@Volume of distribution@L
init: 
  A: 0 @ fooo @ mg/ml
  B: 0 @ yak @ mg/ml
cmt:
  C: hello @ ng
  D: goodbye @ ng/ml
output:
  CP: plasma concentration @ ng/ml
  logR: log10 response @ <none>

'

vec_as_df <- function(x,nm) {
  as_data_frame(setNames(as.list(x),nm)) 
}

a <- parse_yaml(x)



th <- intersect(names(a),c("param", "init", "fixed"))
## Note: because we are using a[th] notation, we are passing in a list
## and we can recover the names of that list
names(a)
a[th] <- lapply(a[th],split3_yaml)
to <- intersect(names(a), c("cmt", "output"))
a[to] <- lapply(a[to], split2_yaml)
b <- a[c(th,to)]


