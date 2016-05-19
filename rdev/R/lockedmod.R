## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


##' Corece an mrgmod object to a lockedmod object.
##'
##' @name as.locked
##' @rdname as.locked
##' @param x mrgmod model object
##' @param dllloc directory location for the model shared object
##' @param dllname the name of the model shared object
##' @param src directory location of the model specification file
##' @param include directory location for the header file
##' @param ... passed along
setGeneric("as.locked", function(x,...) standardGeneric("as.locked"))


##' @export
##' @rdname as.locked
setMethod("as.locked", "mrgmod", function(x,dllloc,dllname,src,include,...) {
    x <- new("lockedmod",
             dllloc=as.character(dllloc),
             dllname=as.character(dllname),
             src=as.character(src),
             x)
    x
})


##' Coerce an mrgmod object to packmod
##' @name as.packmod
##' @rdname as.packmod
##' @param x mrgmod model object
##' @param ... passed along
setGeneric("as.packmod", function(x,...) standardGeneric("as.packmod"))


##' @export
##' @rdname as.packmod
setMethod("as.packmod", "mrgmod", function(x,...) {
    x <- new("packmod",...,x)
    x
})

