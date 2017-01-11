
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
  if(is.language(input)) input <- deparse(input)
  input <- parse_form_3(input)
  do_mutate(data,x=input,...)
})


##' @export
##' @rdname mutate_random
setMethod("mutate_random", c("data.frame", "character"), function(data,input,...) {
  input <- parse_random_string(input)
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
  mutate_random(data,as.list(input),...)
})

parse_right <- function(x) {
  bar <- where_first("|",x)
  op <- where_is("(",x)
  cl <- where_is(")",x)
  if(sum(op>0) != sum(cl>0)) {
    stop("Unmatched parens: ", x)
  }

  lclose <- cl[length(cl)]

  cond <- ""
  if(bar[1] > 0) {
    cond <- substr(x, bar[1], cl[length(cl)])
    x <- sub(cond,"",x,fixed=TRUE)
    x <- paste0(x,")")
  }
  x <- sub("(", "(internal$N,",x,fixed=TRUE)
  cond <- sub("|", "", cond, fixed=TRUE)
  cond <- sub(")", "", cond, fixed=TRUE)
  dist <- substr(x,0,op[1]-1)
  list(cond = cond, call=x,dist=dist,has.cond = nchar(cond) > 0)
}


parse_left_var <- function(x) {
  m <- regexec("(\\w+)(\\[(\\w+)?\\,(\\w+)?\\])?", x)
  m <- unlist(regmatches(x,m))
  var <- m[2]
  bounds <- m[3]
  lower <- m[4]
  upper <- m[5]
  if(lower=="") lower <- -Inf
  if(upper=="") upper <- Inf
  return(list(var=var,bounds=bounds,lower=lower,upper=upper))
}

parse_left <- function(x) {
  x <- unlist(strsplit(x,"+",fixed=TRUE))
  x <- lapply(x,parse_left_var)
  vars <- s_pick(x,"var")
  bounds <- s_pick(x,"bounds")
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

parse_form <- function(x) {
  x <- gsub(" ", "",x, fixed=TRUE)
  form <- strsplit(x,"~",fixed=TRUE)[[1]]
  var <- parse_left(form[1])

  bar <- where_first("|",form[2])

  if(bar > 0) {
    form <- strsplit(form[2], "|", fixed=TRUE)[[1]]
    dist <- form[1]
    by <- form[2]
    by <- unlist(strsplit(by, "[*+]+"),use.names=FALSE)
  } else {
    dist <- form[2]
    by <- character(0)
  }
  c(list(dist=dist,by=by),var)
}

peval <- function(x,envir=list()) {
  eval(parse(text=x),envir=envir)
}

parse_random_block <- function(x) {
  x <- unlist(strsplit(x, "\n",x,fixed=TRUE),use.names=FALSE)
  x <- x[x!=""]
  x <- lapply(x,parse_random_string)
  x
}

mutate_random_list <- function(data,block,...) {

  x <- parse_random_block(block)

  for(i in seq_along(x)) {
    data <- do_mutate(data,x[[i]],...)
  }
  return(data)
}

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

parse_random_string <- function(string) {
  string <- rm_space(string)
  til <- where_first("~",string)
  a <- first_comma(string,til+1)
  if(a > 0) {
    args <- substr(string,a+1,nchar(string))
    args <- peval(paste0("list(",args,")"))
    form <- substr(string,0,a-1)
  } else {
    args <- list()
    form <- string
  }
  form <- parse_form_3(form)
  c(form,list(args=args))
}


parse_3 <- function(x) {

  x <- rm_space(x)

  til <- where_first("~",x)
  bar <- where_first("|",x)
  left <- substr(x,0,til-1)
  a <- first_comma(x,start=til+1)
  group.end <- ifelse(a > 0, a-1, nchar(x))
  right.end <- ifelse(a > 0, a-1, nchar(x))

  if(bar > 0) {
    right <- substr(x,til+1,bar-1)
    group <- substr(x,bar+1,group.end)
  } else {
    if(a > 0) {
      right <- substr(x,til+1,a-1)
    } else {
      right <- substr(x,til+1,nchar(x))
    }
    group <- ""
  }

  if(a > 0) {
    opts <- substr(x,a+1,nchar(x))
  } else {
    opts <-""
  }

  right <- sub("(", "(.n,",right,fixed=TRUE)
  right <- parse(text=right)
  left <- parse_left(left)
  opts <- eval(parse(text=paste0("list(",opts,")")))
  c(left,list(call=right,by=group,opts=opts))
}


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

  right <- parse(text=right)
  left <- parse_left(left)
  c(left,list(call=right,by=group,dist=dist))
}


do_mutate <- function(data,x,envir=list(),tries=10,mult=1.5,...) {
  data <- ungroup(data)

  if(x$dist == "expr") {
    call <- as.character(x$call)
    call <- gsub("expr\\((.+)\\)$", "\\1", call)
    .dots <- paste0("list(~",call,")")
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

  mn <- eval(parse(text=x$lower),envir=envir)
  mx <- eval(parse(text=x$upper),envir=envir)
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
##' @param x a covset
##' @param ... formulae to use for the covset
##' @export
##'
covset <- function(...) {
  x <- list(...)
  y <- as.character(match.call(expand.dots=TRUE))[-1]
  lang <- sapply(x,is.language)
  x[lang] <- lapply(x[lang],deparse)
  x <- lapply(x,as.character)

  new.name <- x==unlist(y,use.names=FALSE)
  if(any(new.name)) {
    n <- sum(new.name)
    y[new.name] <- paste0("x", seq(1,n))
  }

  x <- setNames(x,y)
  return(structure(x,class="covset"))
}

is.covset <- function(x) return(inherits(x,"covset"))

##' @export
##' @rdname covset
setMethod("as.list", "covset", function(x,...) {
  structure(x,class=NULL)
})

##' @export
##' @rdname covset
as.covset <- function(x) {
  if(!is.list(x)) stop("x needs to be a list")
  structure(x,class="covset")  
}

apply_covset <- function(data,.covset,...) {
  for(i in seq_along(.covset)) {
    data <- mutate_random(data,.covset[[i]],...)
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

