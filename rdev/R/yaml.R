

split3_yaml <- function(x,ex) {
  ## x is a list
  if(is.null(x)) return(NULL)
  x <- lapply(x,parse_line,expecting=ex)
  nm <- s_pick(x,"name")
  v <-  s_pick(x,"value")
  v <- tolist(paste(v,collapse=","))
  v <- setNames(v,nm)
  list(v=v,o=list(), nm=nm)
}


gmatch <- function(what,x) as.integer(gregexpr(what,x,fixed=TRUE)[[1]])
msubstr <- function(x,start,end) {
  mapply(start,end,FUN=function(a,b) substr(x,a+1,b-1))
}
cleanup <- function(x,what) {
  what <- what[nchar(what)>0]
  for(i in what) x <- sub(i,"",x,fixed=TRUE)
  x
}
parse_line <- function(x, expecting=3) {
  if(nchar(x)==0) return(NULL)
  col <- gmatch(":",x)
  if(length(col)==1 & expecting==2) x <- sub(":", ": 0 :",x)
  a <- trimws(strsplit(x,":",fixed=TRUE)[[1]])
  length(a) <- 3
  b <- a[3]
  bstart <- gmatch("[",b)
  bend <- gmatch("]",b)
  pstart <- gmatch("(",b)
  pend <- gmatch(")",b)
  
  options <- msubstr(b,bstart,bend)
  units <- msubstr(b,pstart,pend)
  
  b <- cleanup(b,options)
  b <- cleanup(b,units)
  
  b <- gsub("()","",b,fixed=TRUE)
  b <- trimws(gsub("[]","",b,fixed=TRUE))
  
  list(name=a[1],value=a[2],unit=units,options=options)
}



# block <- '
# CL: 2 : Clearance (L/hr)
# VC: 300 : Volume (L)
# KA: 4/2 : (hey)
# '
# x <- strsplit(block,"\n")[[1]]
# a <- split3_yaml(x,3)

