
render_annot <- function(x,block,...) {
  dplyr::bind_rows(lapply(x,dplyr::as_data_frame)) %>%
    dplyr::mutate(block=block) %>% 
    dplyr::select(block,dplyr::everything()) %>%
    as.data.frame
}


parse_annot <- function(x,noname=FALSE,novalue=FALSE,block='.',name_value=TRUE,...) {
  ## x is a list
  if(is.null(x)) return(NULL)
  x <- x[nchar(x)>0]
  x <- lapply(x,parse_annot_line,novalue=novalue,noname=noname)
  nm <- s_pick(x,"name")
  v <-  s_pick(x,"value")

  if(name_value) {
    v <- setNames(tolist(paste(v,collapse=","),...),nm)
  } else {
    v <- as.numeric(tovec(v)) 
  }
  an <- lapply(x,"[", c("name","descr", "unit","options"))
  an <-  render_annot(an,block)
  list(v=v,an=an,nm=nm)
}

## Convenience; keep around for a little bot
gmatch <- function(what,x) as.integer(gregexpr(what,x,fixed=TRUE)[[1]])

parse_annot_line <- function(x, novalue=FALSE, noname=FALSE) {
  
  if(nchar(x)==0) return(NULL)
  
  x <- mytriml(x)
  
  col <- charcount(x,":")[1]
  
  if(col != (2-noname-novalue)) {
    stop("Improper model annotation: ", x, call.=FALSE) 
  }
  
  ## Fix up line if not name : value : other
  if(noname) x <- paste0(". :",x)
  if(novalue) x <- gsub(":",": 0 :",x,fixed=TRUE)
  
  a <- strsplit(x,"\\s*:\\s*",perl=TRUE)[[1]]
  
  b <- a[3]
  
  ## grep out units and options
  units <- regmatches(b,gregexpr("\\s*\\(.*?\\)",b))[[1]]
  options <- regmatches(b,gregexpr("\\s*\\[.*?\\]",b))[[1]]
  
  if(length(units)   > 1) units   <- units[length(units)]
  if(length(options) > 1) options <- options[length(options)]
  
  ## Drop matches
  for(what in c(units,options)) b <- sub(what,"",b,fixed=TRUE)  
  
  ## Clean up matches
  units <-   gsub("\\s*\\(\\s*|\\s*\\)", "", units,   perl=TRUE)
  options <- gsub("\\s*\\[\\s*|\\s*\\]", "", options, perl=TRUE)

  
  ## This is the "description"
  b <- mytrim(b)
  
  if(length(units)==0) units <- '.'
  if(length(options)==0) options <- '.'
  
  list(name=a[1],value=a[2],unit=units,options=options,descr=b)
}


details <- function(x,...) {
  stopifnot(is.mrgmod(x))
  return(x@annot[["data"]])  
}

store_annot <- function(x,what,loc=soloc(x),...) {
  stopifnot(is.mrgmod(x))
  x@annot <- list(data=what,embedded=TRUE)
  x
}




