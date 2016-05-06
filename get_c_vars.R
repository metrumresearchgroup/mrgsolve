##' ---
##' output: 
##'   md_document:
##'     variant: markdown_github
##' ---

#+ message=FALSE
library(dplyr)
library(magrittr)
library(mrgsolve)

x <- "double x = 2;long double a= 2;  bool foo_yak = 34 + pow(THETA1,THETA2);"

a <- mrgsolve:::get_sep_tokens(x) %>% unlist



fails <- '
$PARAM CL = 0.3, VC = 1.5, wt = 5
$CMT CENT
$MAIN
double a = 2;   localbool z= 1;int j  = 123; double m =2; double    zz = 2;
double a=2 , bool zzz  = 2;
double CL_i = CL*exp(ETA(1))*wt/5;
double Vi = VC*exp(ETA(2));
$ODE
dxdt_CENT = 0;
 double setinode = pow(a,b);
$OMEGA
0.1
0.04
$TABLE
bool  IPRED = 2/3;
double b = 2;
$CAPTURE CL_i Vi wt
'

y <- strsplit(fails, "\n")[[1]]


get_c_vars <- function(y) {
  
  ## Lines matching 
  m <- gregexpr("\\b(double|int|bool) \\s*\\w+\\s*=",y,perl=TRUE)
  what <- regmatches(y,m)
  keep <- lapply(what,length)
  whic <- lapply(seq_along(keep), function(i) {
    rep(i,length(what[[i]]))
  }) %>% unlist
  
  what <- unlist(what)
  m2 <- gregexpr("(double|int|bool)", what,perl=TRUE)

  remain <- regmatches(what,m2,invert=TRUE)
  remain <- lapply(remain, `[`,2) 
  remain <- lapply(remain, gsub, pattern="^\\s+", replacement="") %>% unlist
  var <- gsub("\\s*=$", "", remain)
  dec <- gsub("\\s*=$", "", what)
  dec <- paste0(gsub("\\s+", " ",dec), ";")
  data_frame(line = whic, from=what, to=remain,var=var, dec=dec)
}


x <- readLines("~/project.mrg/amg/416/script/model/model2.14.0.cpp")
x <- x[!grepl("^\\s*//",x)]

system.time({find <- get_c_vars(x)})

library(metrumrg)
system.time(mod <- mread("model2.14.0", project="~/project.mrg/amg/416/script/model/"))



