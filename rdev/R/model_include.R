includes <- new.env()
includes[["Rcpp"]] <- function() {
  list(code=c("#include <Rcpp.h>","#undef table"),
       CLINK = mrgsolve:::link_to("Rcpp"))
}
includes[["BH"]] <- function() {
  list(CLINK = mrgsolve:::link_to("BH"))
}
includes[["RcppArmadillo"]] <- function() {
  list(CLINK = mrgsolve:::link_to("RcppArmadillo"))
}
includes[["DIST"]] <- function() {
  list(code="#include \"MRGSOLVE_DIST.h\"",
       CLINK = mrgsolve:::link_to("mrgsolve"),
       depends=c("Rcpp"))
}


link_to <- function(package) {
    path <- find.package(package, quiet=TRUE)
    path <- file.path(path,"include")
    return(paste0("-I\"",path,"\""))
}

get_include <- function(name) {
    if(!exists(name,includes)) {
        stop("Model include: ", name, " could not be found.")
    }
    includes[[name]]()
}

get_includes <- function(what) {
    x <- lapply(what,get_include)
    dep <- s_pick(x,"depends")
    need <- setdiff(dep,what)
    x <- c(lapply(need,get_include),x)
    names(x) <- c(what,need)
    x
}

reshape_includes <- function(x) {
    data <- x
    clink <- paste(s_pick(x,"CLINK"),collapse=" ")
    restore <- list(CLINK_CPPFLAGS = Sys.getenv("CLINK_CPPFLAGS", unset=NA))
    code <- s_pick(x,"code")
    new_env <- list(CLINK_CPPFLAGS = clink)

    list(data=data,clink=clink, code=code,restore=restore, new_env=new_env)


}

new_env <- function(x) {
    return(invisible(do.call(Sys.setenv,x$new_env)))
}

restore_env <- function(x) {
    return(invisible(do.call(Sys.setenv,x$restore)))
}




