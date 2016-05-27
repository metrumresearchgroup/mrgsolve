includes <- new.env()

includes[["Rcpp"]] <- function() {
  list(code=c("#include <Rcpp.h>"),
       CLINK = mrgsolve:::link_to("Rcpp"))
}

includes[["BH"]] <- function() {
  list(CLINK = mrgsolve:::link_to("BH"))
}

includes[["RcppArmadillo"]] <- function() {
  list(CLINK = paste0(mrgsolve:::link_to("RcppArmadillo")," ",
                      mrgsolve:::link_to("Rcpp")),
       code=c(#define ARMA_DONT_USE_CXX11",
         "#include <RcppArmadillo.h>",
         "#define NDEBUG 1")
  )
}

includes[["DIST"]] <- function() {
  list(code=c("#include \"MRGSOLVE_DIST.h\""),
       CLINK = mrgsolve:::link_to("mrgsolve"),
       depends=c("Rcpp"))
}

includes[["MVNORM"]] <- function() {
  list(code=c("#include \"MVNORM.h\""),
       CLINK = mrgsolve:::link_to("mrgsolve"),
       depends=c("RcppArmadillo"))
}

#  Construct a path to a package to add to CLINK_CPPFLAGS
link_to <- function(package) {
    path <- find.package(package, quiet=TRUE)
    path <- file.path(path,"include")
    return(paste0("-I\"",path,"\""))
}
# Get one include
get_include <- function(name) {
    if(!exists(name,includes)) {
        stop("Model include: ", name, " could not be found.")
    }
    includes[[name]]()
}
# Get all includes, include dependencies
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

    clink <- paste(unique(s_pick(x,"CLINK")),collapse=" ")
    new_env <- list(CLINK_CPPFLAGS = clink)
    restore <- list(CLINK_CPPFLAGS = Sys.getenv("CLINK_CPPFLAGS", unset=NA))

    code <- s_pick(x,"code")

    list(data=data,clink=clink,code=code,restore=restore, new_env=new_env)
}

modify_env <- function(x) {
    if(length(x$new_env)==0) return(invisible(NULL))
    return(invisible(do.call(Sys.setenv,x[["new_env"]])))
}

restore_env <- function(x) {
    # So far there are only unset
    #restore <- x$restore[!is.na(x[["restore"]])]
    #unset <- names(x[["restore"]][is.na(x[["restore"]])])
    #if(length(restore)) do.call(Sys.setenv,restore)
    #if(length(unset)) Sys.unsetenv(unset)
    Sys.unsetenv("CLINK_CPPFLAGS")
    return(invisible(NULL))
}



