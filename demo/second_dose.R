##' ---
##' output: 
##'   md_document:
##'     variant: markdown_github
##' ---
#+ message=FALSE
library(mrgsolve)
library(dplyr)
#+ echo=FALSE
library(knitr)
opts_chunk$set(fig.path="img/second_dose-")


##' Use the house model
mod <- mrgsolve:::house() %>% update(delta=0.1) %>% Req(CP)

##' The default time grid
mod %>% 
  ev(amt=100,ii=24,addl=1) %>%
  mrgsim %>%
  plot

##' We can start at 24 and end at 48; but this doesn't 
##' quite look right; we still get the dose at `time=0`
mod %>% 
  ev(amt=100,ii=24,addl=1) %>%
  mrgsim(start=24, end=48) %>%
  plot


##' Drop the dose records from the output
mod %>% 
  ev(amt=100,ii=24,addl=1) %>%
  obsonly %>%
  mrgsim(start=24, end=48) %>%
  plot


##' Another way to do it: set `end=-1` to get 
##' rid of that simulation time grid and
##'  the give `mrgsolve` 
##' an ad-hoc vector of times (`add`) to output.
##' 
##' You will still have to drop the dosing record to 
##' avoid seeing that record in the output.
##' 
##'  
mod %>% 
  ev(amt=100,ii=24,addl=1) %>%
  obsonly %>%
  mrgsim(end=-1, add=seq(24,48,0.1)) %>%
  plot


##' Via data set
##' 
##' 
data <- expand.ev(time=seq(0,48,1), amt=0,evid=0) %>% mutate(ID=1)
data <- bind_rows(data, data %>% mutate(ID=2))
doses <- data %>% filter(time==0) %>% mutate(amt=100,evid=1,cmt=1)
data <- bind_rows(doses,data) %>% arrange(ID,time,evid)
data <- data %>% filter(ID==1 & time <= 24 | (ID==2 & time >= 24 | evid==1))

#+
mod %>% 
  data_set(data) %>%
  obsonly %>%
  mrgsim() %>%
  plot(CP~time|ID)


##' 
sessionInfo()