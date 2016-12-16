##' @export
##' @rdname matlist-class
setClass("omegalist", contains="matlist")

##' @export
##' @rdname matlist-class
setClass("sigmalist", contains="matlist")

##' S4 parameter_list class
##' @details
##' parameter_list is a \code{\link{numericlist-class}}
setClass("parameter_list",contains="numericlist")

##' S4 cmt_list class
##' @details
##' cmt_list is a \code{\link{numericlist-class}}
setClass("cmt_list",contains="numericlist")


