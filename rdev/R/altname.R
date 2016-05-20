
make_altnames <- function(from,to) {
  if(missing(to)) to <- from
  paste(to,from,sep="=")
}

set_altname <- function(x) {
  if(length(x)==0) return(as.character(x))
  y <- strsplit(as.character(x),"\\s*=\\s*")
  to <- sapply(y,`[`,1)
  from <- sapply(y,`[`,2)
  from <- ifelse(is.na(from), to, from)
  if(identical(from,to)) return(as.character(from))
  return(structure(list(from=from,to=to,rename=!identical(from,to)),class="altname"))
  
}

altname <- function(x,...) UseMethod("altname")
altname.default <- function(x,y,...) return(y)
altname.altname <- function(x,y, ...) {
  old <- match(y,x[["from"]])
  old <- sort(old[!is.na(old)])
  nw <- match(x[["from"]],y)
  nw <- nw[!is.na(nw)]
  y[nw] <- x[["to"]][old]
  return(y)
}
as.character.altname <- function(x,...) {
  as.character(x[["from"]])
}

