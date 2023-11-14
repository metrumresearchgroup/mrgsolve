#' --- 
#' output: md_document
#' ---
#' 
#' 
#' Some ad-hoc tests of doses, alag, and bioavailability
#' 
library(mrgsolve)
library(dplyr)
library(testthat)

code <- '
$SET Req = "CP", end = 72, delta = 0.1

$PARAM F1 = 1, D1 = 5, ALAG1 = 0, R1 = 0

$PKMODEL cmt = "CENT"

$PK
double CL = 1; 
double V  = 10;
double KA = 1.2;
F_CENT = F1;
D_CENT = D1;
ALAG_CENT = ALAG1;
R_CENT = R1;

$ERROR
capture CP = CENT/V;
'

cmax <- function(data) {
  data <- as.data.frame(data)
  data <- filter(data, CP==max(CP))
  data$time
}

mod <- mcode("f-dot-r", code)
dose <- ev(amt = 100, tinf = 5)

#' Base case: duration  = tinf
a <- mod %>% mrgsim(dose) %>% cmax()
a == 5

#' Reduce F by half reduced infusion by half
b <- mod %>% param(F1 = 0.5) %>% mrgsim(dose) %>% cmax()
b == a / 2

#' Set infusion rate by R1
doser <- ev(amt = 100, rate = -1, R1 = 50)
c <- mod %>% mrgsim(doser) %>% cmax()
c == 2

#' Set infusion rate by R1 with new F1
dosefr <- ev(amt = 100, rate = -1, R1 = 50, F1 = 3)
d <- mod %>% mrgsim(dosefr) %>% cmax()
d == c * 3

#' Set infusion rate by D1
dosed <- ev(amt = 100, rate = -2, D1 = 7)
e <- mod %>% mrgsim(dosed) %>% cmax()
e == 7

#' Set infusion rate by D1 with new F1
dosefd <- ev(amt = 100, rate = -2, D1 = 7, F1 = 0.2)
f <- mod %>% mrgsim(dosefd) %>% cmax()
f == 7

#' Set infusion rate by R1 with ALAG
doserl <- ev(amt = 100, rate = -1, R1 = 50, ALAG1 = 1)
g <- mod %>% mrgsim(doserl) %>% cmax()
g == c + 1

#' Set infusion with ALAG
doseal <- ev(amt = 100, tinf = 10, ALAG1 = 5)
h <- mod %>% mrgsim(doseal) %>% cmax()
h == 15

#' Set infusion with ALAG and D1
doseda <- ev(amt = 100, rate = -2, D1 = 3, ALAG1 = 5)
i <- mod %>% mrgsim(doseda) %>% cmax()
i == 8

#' Set infusion with ALAG and R1
dosera <- ev(amt = 100, rate = -1, R1 = 20, ALAG1 = 2.5)
j <- mod %>% mrgsim(dosera) %>% cmax()
j == 7.5

