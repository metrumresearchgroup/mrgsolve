
render_annot <- function(x,block,...) {
  dplyr::bind_rows(lapply(x,dplyr::as_data_frame)) %>%
    dplyr::mutate(block=block) %>% 
    dplyr::select(block,dplyr::everything()) %>%
    as.data.frame
}


parse_annot <- function(x,noname=FALSE,novalue=FALSE,block='.') {
  ## x is a list
  if(is.null(x)) return(NULL)
  x <- x[nchar(x)>0]
  x <- lapply(x,parse_annot_line,novalue=novalue,noname=noname)
  nm <- s_pick(x,"name")
  v <-  s_pick(x,"value")
  v <- setNames(tolist(paste(v,collapse=",")),nm)
  an <- lapply(x,"[", c("name","descr", "unit","options"))
  an <-  render_annot(an,block)
  list(v=v,an=an,nm=nm)
}

## Convenience; keep around for a little bot
gmatch <- function(what,x) as.integer(gregexpr(what,x,fixed=TRUE)[[1]])

parse_annot_line <- function(x, novalue=FALSE,noname=FALSE) {
  
  if(nchar(x)==0) return(NULL)
  
  x <- trimws(x,"left")
  
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
  units <- gsub("\\s*\\(\\s*|\\s*\\)", "",units,perl=TRUE)
  options <- gsub("\\s*\\[\\s*|\\s*\\]", "",options,perl=TRUE)
  
  
  ## This is the "description"
  b <- trimws(b)
  
  if(length(units)==0) units <- '.'
  if(length(options)==0) options <- '.'
  
  list(name=a[1],value=a[2],unit=units,options=options,descr=b)
}


details <- function(x,...) {
  
  stopifnot(is.mrgmod(x))
  
  if(!is.character(x@annot[["file"]])) {
    warning("Could not find location of detail information.", call.=FALSE) 
    return(list())
  }
  if(!file.exists(x@annot[["file"]])) {
    warning("File with detail information does not exist.", call.=FALSE) 
    return(list())
  }
  readRDS(file=x@annot[["file"]])
}

store_annot <- function(x,what,loc=soloc(x),...) {
  file_name <- file.path(loc,paste0(model(x),"-details.RDS"))
  saveRDS(file=file_name,what)
  return(list(file=file_name))
}


