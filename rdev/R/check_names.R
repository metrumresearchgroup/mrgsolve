
check_names <- function(x,par,cmt) {
  
  x <- x[!is.element(x,c(".", "..."))]
  
  dups <- any(duplicated(x))
  us <- any(grepl("\\_",x,perl=TRUE))
  res <- any(is.element(x,Reserved))
  
  ans <- character(0)
  
  ## Duplicate names are not allowed
  if(dups) {
    tmp <- paste(x[duplicated(x)], collapse=" ")
    ans <- c(ans,paste0("Duplicated model names: ", tmp))
  }
  ## Look for names in the Reserved word list
  if(res) {
    tmp <- paste(x[is.element(x,Reserved)],collapse=" ")
    ans <- c(ans,paste0("Reserved words in model names: ",tmp))
  }
  ## Scan for names with underscores
  ## Leading underscores are not allowed
  ## Also, look for any name that conflicts with
  ##   bioav, lag-time, or infusion duration or ate
  if(us) {
    u <- grep("\\_",x,perl=TRUE,value=TRUE)
    leading <- grep("^\\_",x,perl=TRUE,value=TRUE)
    if(length(leading) > 0) {
      ans <- c(ans, paste0("Leading underscore not allowed: ", paste(leading, collapse=" "))) 
    }
    check <- as.character(sapply(c("F_", "ALAG_", "D_", "R_"),paste0,cmt))
    iv_name <- intersect(x,check)
    if(length(iv_name) > 0) {
      ans <- c(ans, paste0("Reserved symbols in model names: ", iv_name))
    }
  }
  return(ans)
}
