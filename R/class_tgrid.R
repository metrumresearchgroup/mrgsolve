

##' @export
##' @rdname stime
setClass("tgrid", slots=c(start="numeric", end="numeric", delta="numeric", add="numeric", offset="numeric", scale="numeric"),
         prototype=list(start=0, end=24, delta=1, offset=0,scale=1))

##' @export
##' @rdname stime
setClass("tgrids", slots=c(data="list"))


##' Create a list of designs from a data frame.
##' 
##' @param data input data set; see details
##' @param descol character column name to be used for design groups
##' 
##' @details
##' The input data set must have a column with the same name as the value 
##' of \code{descol}.  Other column names should be \code{start} (the time 
##' of the first observation), \code{end} (the time of the last observation), 
##' \code{delta} (the time steps to take between \code{start} and \code{end}), 
##' and \code{add} (other, ad-hoc times).  Note that \code{add} might be 
##' a \code{list-column} to get a vector of times for each time grid object.
##' 
##' @return The function returns a list of \code{tgrid} objects, 
##' one for each unique value found in \code{descol}.
##' 
##' @examples
##' idata <- data_frame(ID=1:4, end=seq(24,96,24), delta=6,
##' add=list(c(122,124,135),c(111), c(99),c(88)))
##' 
##' idata <- mutate(idata, GRP = ID %%2)
##' 
##' l <- as_deslist(idata,"GRP")
##' 
##' l
##' 
##' lapply(l,stime)
##' 
##' lapply(as_deslist(idata, "ID"),stime)
##' 
##' @export
as_deslist <- function(data,descol="ID") {
  
  if(!is.data.frame(data)) {
    stop("data must be a data frame", call.=FALSE) 
  }
  if(!is.element("end", names(data))) {
    stop("end is a required column for input data", call.=FALSE) 
  }
  if(!is.element("delta", names(data))) {
    data <- data %>% mutate(delta = 1) 
  }
  if(!is.element("start", names(data))) {
    data <- data %>% mutate(start=0) 
  }
  if(!is.element("add", names(data))) {
    data <- data %>% mutate(add=0)
  }
  
  designs <- distinct_(data,.dots=descol,.keep_all=TRUE)
  
  designs <- mutate(group_by_(designs,.dots=descol),
                    des=list(tgrid(start=first(start),
                                   end=first(end),
                                   delta=first(delta),
                                   add=unlist(add[1]))))
  
  return(designs$des)
}


