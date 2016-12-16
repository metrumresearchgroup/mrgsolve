library(metrumrg)
library(plyr)
library(dplyr)
library(mrgsolve)


set.seed(112233)
NID <- 10
TV <- c(CL=1, VC=20, KA=0.1, KOUT=2,IC50=2)
OMEGA <- diag(c(0.5, 0.5, 1,0.2,0.4))

pars <- signif(exp(mvrnorm(NID, log(TV), OMEGA)),3)
#pars

idata <- as.data.frame(pars)
idata$FOO <- sample(1:NID)
idata$ID <- 1:nrow(idata)
idata <- mrgsolve:::shuffle(idata, "ID")
#idata


doses <- list(data.frame(ID=1,amt=1000, cmt=1, time=0,addl=3,ii=24),
              data.frame(ID=2,amt=1000,rate=20, time=0,cmt=2),
              data.frame(ID=3, amt=c(1000,500,500,1000), time=c(0,24,48,72),cmt=1),
              data.frame(ID=4,amt=2000, rate=100,ii=48, addl=2,cmt=2),
              data.frame(ID=5,amt=c(1000,5000),rate=c(0,60), time=c(0,24), cmt=1))

doses <- bind_rows(doses) %>% as.data.frame
doses$evid <- 1
doses[] <- mapply(doses, FUN=function(x) {x[is.na(x)] <- 0; x})
#doses

if(require(mrgsolve)) {
mod <- mrgsolve:::house(end=160,delta=1)
out <- mod %>% data_set(doses) %>% mrgsim
#plot(out,CP~time|factor(ID))
}

tim <- seq(0,160,1)

data <- ddply(doses, .(ID), function(x) {
    x <- x[1,,drop=FALSE]
    x$evid <- 0
    x <- x[rep(1,length(tim)),]
    x$time <- tim
    x
})
data <- within(data, {
  obs <- evid==0
  amt[obs] <- 0
  cmt[obs] <- 0
  rate[obs] <- 0
  ii[obs] <- 0
  addl[obs] <- 0
  rm(obs)
})


data <- rbind(data, doses)
data <- data[order(data$ID,data$time,1-data$evid),]
data <- subset(data, ID !=2 | (ID==2 & time <= 60))
data <- mrgsolve:::shuffle(data, c("ID", "time", "cmt","evid","amt"))
rownames(data) <- NULL
#head(data)


data2 <- merge(data,idata[,c("ID","CL", "VC", "KA")], by="ID", sort=FALSE, all.x=TRUE, all.y=FALSE)
#head(data2)

exidata <- idata
save(exidata,file="data/exidata.RData")

extran1 <- doses
save(extran1, file="data/extran1.RData")

extran2 <- data
save(extran2, file="data/extran2.RData")

extran3 <- data2
save(extran3,file="data/extran3.RData")

data(Theoph)
dd <- as.data.frame(Theoph)
colname(dd) <- c(Subject="ID", Time ="time" , Wt="WT")
dd$ID <- as.numeric(as.character(dd$ID))
dd$evid <- dd$amt <- dd$cmt <-  0
dd$evid[dd$time==0] <- 1
dd$amt[dd$evid==1] <- dd$Dose[dd$evid==1]
dd$cmt[dd$evid==1] <- 1
dd$conc[dd$time==0] <- 0

exTheoph <- dd
save(exTheoph, file="data/exTheoph.RData")


library(MASS)
library(metrumrg)
theta <- c(1,10,1.2)
runs <- 100

require(mrgsolve)
omega <- cmat(c(0.1, 0.2, 0.2, 0.6, 0.1, 0.3))

pars <- as.data.frame(simpar(100,log(theta), diag(c(0.15, 0.25, 0.5)),
               omega = omega, sigma=diag(c(0.002, 1)),
               odf=30, sdf=100))
pars$SG2.1 <- 0
#head(pars)
names(pars) <- gsub("\\.", "", names(pars))
names(pars) <- gsub("TH", "THETA", names(pars))
names(pars) <- gsub("OM", "OMEGA", names(pars))
names(pars) <- gsub("SG", "SIGMA", names(pars))
pars$run <- 1:nrow(pars)
pars <- shuffle(pars, "run")
exBoot <- pars
save(exBoot, file="data/exBoot.RData")










