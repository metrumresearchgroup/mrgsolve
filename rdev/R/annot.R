

parse_annot <- function(x,noname=FALSE,novalue=FALSE) {
  ## x is a list
  if(is.null(x)) return(NULL)
  x <- lapply(x,parse_annot_line,novalue=novalue,noname=noname)
  nm <- s_pick(x,"name")
  v <-  s_pick(x,"value")
  v <- tolist(paste(v,collapse=","))
  v <- setNames(v,nm)
  list(v=v,o=list(), nm=nm)
}

## Convenience; keep around for a little bot
gmatch <- function(what,x) as.integer(gregexpr(what,x,fixed=TRUE)[[1]])

parse_annot_line <- function(x, novalue=FALSE,noname=FALSE) {
  
  if(nchar(x)==0) return(NULL)
  
  col <- gmatch(":",x)
  
  if(length(col) != (2-noname-novalue)) {
    stop("Improper model annotation: ", x, call.=FALSE) 
  }
  
  ## Fix up line if not name : value : other
  if(noname) x <- paste0(". :",x)
  if(novalue) x <- gsub(":", ": 0 :",x)
  
  a <- strsplit(x,"\\s*:\\s*",perl=TRUE)[[1]]
  
  b <- a[3]
  
  ## grep out units and options
  units <- regmatches(b,gregexpr("\\s*\\(.*?\\)",b))[[1]]
  options <- regmatches(b,gregexpr("\\s*\\[.*?\\]",b))[[1]]
  
  ## Drop matches
  for(what in c(units,options)) b <- gsub(what,"",b,fixed=TRUE)  
  
  ## Clean up matches
  units <- gsub("\\s*\\(|\\)", "",units,perl=TRUE)
  options <- gsub("\\s*\\[|\\]", "",options,perl=TRUE)
  
  ## This is the "description"
  b <- trimws(b)
  
  list(name=a[1],value=a[2],unit=units,options=options,descr=b)
}

