

##' @export
##' @rdname stime
setClass("tgrid", slots=c(start="numeric", end="numeric", delta="numeric", add="numeric", offset="numeric", scale="numeric"),
         prototype=list(start=0, end=24, delta=1, offset=0,scale=1))

##' @export
##' @rdname stime
setClass("tgrids", slots=c(data="list"))


