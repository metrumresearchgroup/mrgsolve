
setClass("covset")

##' Add random variates to a data frame.
##'
##' @param data the data.frame to mutate
##' @param input an unquoted R formula; see details.
##' @param ... additional inputs
##'
##' @export
##' @importFrom dplyr left_join bind_cols data_frame select_ mutate_ ungroup
##' @importFrom stats rbinom setNames
##' @importFrom utils type.convert
##' @importFrom methods setGeneric
setGeneric("mutate_random", function(data,input,...) standardGeneric("mutate_random"))


##' @export
##' @rdname mutate_random
##'
setMethod("mutate_random", c("data.frame","formula"), function(data,input,...) {
  input <- new_covobj(input)
  do_mutate(data,x=input,...)
})


##' @export
##' @rdname mutate_random
setMethod("mutate_random", c("data.frame", "character"), function(data,input,...) {
  input <- new_covobj(input)
  args <- input$args
  input$args <- NULL
  args <- c(args,list(x=input,data=data),list(...))
  do.call(do_mutate,args)
})

##' @export
##' @rdname mutate_random
setMethod("mutate_random", c("data.frame", "list"), function(data,input,...) {
  apply_covset(data,input,...)
})
##' @export
##' @rdname mutate_random
setMethod("mutate_random", c("data.frame", "covset"), function(data,input,...) {
  apply_covset(data,input,...)
})
##' @export
##' @rdname mutate_random
setMethod("mutate_random", c("data.frame", "covobj"), function(data,input,...) {
  do_mutate(data,input,...)
})



parse_left_var <- function(x) {
  m <- regexec("(\\w+)(\\[(\\w+)?\\,(\\w+)?\\])?", x)
  m <- unlist(regmatches(x,m))
  var <- m[2]
  bounds <- m[3]
  lower <- m[4]
  upper <- m[5]
  if(lower=="") lower <- -Inf
  if(upper=="") upper <- Inf
  return(list(var=var,lower=Parse(lower),upper=Parse(upper)))
}

parse_left <- function(x) {
  x <- unlist(strsplit(x,"+",fixed=TRUE))
  x <- lapply(x,parse_left_var)
  vars <- s_pick(x,"var")
  lower <- s_pick(x,"lower")
  upper <- s_pick(x,"upper")
  list(vars=vars,lower=lower,upper=upper,n=length(vars))
}


bound <- function(call,n,envir=list(),mult=1.3,mn=-Inf,mx=Inf,tries=10) {
  n0 <- n
  n <- n*mult
  ngot <- 0
  y <- numeric(0)
  envir$.n <- n
  for(i in seq(1,tries)) {
    yy <- eval(call,envir=envir)
    yy <- yy[yy > mn & yy < mx]
    ngot <- ngot + length(yy)
    y <- c(yy,y)
    if(ngot > n0) break
  }
  if(ngot < n0) {
    stop("Could not simulate required variates within given bounds in ", tries, " tries", call.=FALSE) 
  }
  return(y[1:n0])
}

rbinomial <- function(n,p,...) rbinom(n,1,p)

first_comma <- function(x,start=1) {
  open <- 0
  where <- NULL
  for(i in start:nchar(x)) {
    a <- substr(x,i,i)
    if(a=="(")  {
      open <- open+1
      next
    }
    if(a==")") {
      open <- open-1
      next
    }
    if(a=="," & open==0) return(i)
  }
  return(-1)
}

rm_space <- function(x) gsub(" ", "",x,fixed=TRUE)
peval <- function(x) eval(parse(text=x))

# parse_random_string <- function(string) {
#   string <- rm_space(string)
#   til <- where_first("~",string)
#   a <- first_comma(string,til+1)
#   if(a > 0) {
#     args <- substr(string,a+1,nchar(string))
#     args <- peval(paste0("list(",args,")"))
#     form <- substr(string,0,a-1)
#   } else {
#     args <- list()
#     form <- string
#   }
#   form <- parse_form_3(form)
#   c(form,list(args=args))
# }
# 

parse_form_3 <- function(x) {

  x <- rm_space(x)

  til <- where_first("~",x)
  bar <- where_first("|",x)
  left <- substr(x,0,til-1)


  if(bar > 0) {
    right <- substr(x,til+1,bar-1)
    group <- substr(x,bar+1,nchar(x))
  } else {
    right <- substr(x,til+1,nchar(x))
    group <- ""
  }

  op <- where_first("(",right)
  dist <- substr(right,0,op-1)

  if(substr(dist,0,1)=="r") {
    right <- sub("(", "(.n,",right,fixed=TRUE)
  }
  
  if(dist=="expr") {
    right <- as.character(right)
    right <- gsub("expr\\((.+)\\)$", "\\1", right)
  }

  right <- parse(text=right)
  left <- parse_left(left)
  c(left,list(call=right,by=group,dist=dist))
}


# @param data a data frame
# @param x a covobj
do_mutate <- function(data,x,envir=list(),tries=10,mult=1.5,...) {
  
  data <- ungroup(data)

  if(call_type(x)==2) {
    .dots <- paste0("list(~",x$call,")")
    .dots <- eval(parse(text=.dots),envir=envir)
    names(.dots) <- x$vars
    data <- mutate_(data, .dots=.dots)
    return(data)
  }

  if(tries <=0) stop("tries must be >= 1")

  x$by <- c(x$by,x$opts$by)
  x$by <- x$by[x$by != ""]

  has.by <- any(nchar(x$by) > 0)

  if(has.by) {
    skele <- dplyr::distinct_(data,.dots=x$by)
    n <- nrow(skele)
  } else {
    n <- nrow(data)
  }

  mn <- eval(x$lower,envir=envir)
  mx <- eval(x$upper,envir=envir)
  r <- data_frame(.x=bound(x$call,n=n,mn=mn, mx=mx,tries=tries,envir=envir))
  names(r) <- x$vars
  data <- select_(data,.dots=setdiff(names(data),names(r)))

  if(has.by) {
    r <- bind_cols(skele,r)
    return(left_join(data,r,by=x$by))
  } else {
    return(bind_cols(data,r))
  }

}


##' Create a set of covariates.
##' @param ... formulae to use for the covset
##' @export
##'
covset <- function(...) {
  x <- list(...)
  x <- lapply(x,new_covobj)
  return(structure(x,class="covset"))
}


is.covset <- function(x) return(inherits(x,"covset"))

##' @export
##' @rdname covset
as.covset <- function(x) {
  if(!is.list(x)) stop("x needs to be a list")
  structure(x,class="covset")  
}

apply_covset <- function(data,.covset,...) {
  for(i in seq_along(.covset)) {
    data <- do_mutate(data,.covset[[i]],...)
  }
  return(data)
}

get_covsets <- function(x) {
  if(is.environment(x)) {
    x <- as.list(x) 
  }
  cl <- sapply(x,class)
  x[cl=="covset"]
}

Parse <- function(x) parse(text=x)

