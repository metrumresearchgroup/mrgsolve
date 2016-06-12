## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


##' @include utils.R


generate_rdefs <- function(pars,
                           cmt,
                           func,
                           init_fun="MRGSOLVE_NO_INIT_FUN",
                           table_fun="MRGSOLVE_NO_TABLE_FUN",
                           config_fun  = "",
                           model="",omats,smats,parsedata=list(),...) {

    npar <- length(pars)
    ncmt <- length(cmt)

    dxdt <- paste0("dxdt_",cmt)
    init <- paste0(cmt, "_0")

    cmtindex <- (1:length(cmt))-1
    parsindex <- (1:length(pars))-1

    cmtdef <-  paste0("#define ", cmt,  " _A_[",    cmtindex,"]")
    initdef <- paste0("#define ", init, " _A_0_[",  cmtindex,"]")
    dxdef <-   paste0("#define ", dxdt, " _DADT_[", cmtindex,"]")
    pardef <-  paste0("#define ", pars, " _THETA_[",parsindex,"]")

    etal <- epsl <- NULL

    if(sum(nrow(omats)) > 0) {
        etai <- 1:sum(nrow(omats))
        etal <- unlist(omats@labels)
        stopifnot(length(etai)==length(etal))
        which <- etal != "."
        if(sum(which) > 0) {
            etal <- paste0("#define ", etal[which], " ETA(",etai[which],")")
        } else {
            etal <- NULL
        }
    }

    if(sum(nrow(smats)) > 0) {
        epsi <- 1:sum(nrow(smats))
        epsl <- unlist(smats@labels)
        stopifnot(length(epsi)==length(epsl))
        which <- epsl != "."
        if(sum(which) > 0) {
            epsl <- paste0("#define ", epsl[which], " EPS(",epsi[which],")")
        } else {
            epsl <- NULL
        }
    }

    Fdef <- Adef <- Ddef <- Rdef <- cmtndef <- NULL

    cmtn <- unique(intersect(as.cvec(parsedata$CMTN),cmt))

    if(length(cmtn)>0) {
        cmtnindex <- (match(cmtn,cmt))
        cmtndef <- paste0("#define ", paste0("N_", cmtn), " ", cmtnindex)
        Fdef <- paste0("#define ", paste0("F_",cmtn), " _F_[",cmtnindex-1,"]")
        Adef <- paste0("#define ", paste0("ALAG_",cmtn), " _ALAG_[",cmtnindex-1,"]")
        Ddef <- paste0("#define ", paste0("D_",cmtn), " _D_[",cmtnindex-1,"]")
        Rdef <- paste0("#define ", paste0("R_",cmtn), " _R_[",cmtnindex-1,"]")
    }

    if(npar==0) pardef <- ""
    if(ncmt==0) cmtdef <- initdef <- dxdef <- ""

    return(
        c(paste0("#define INITFUN___ ",init_fun),
          paste0("#define ODEFUN___ ",func),
          paste0("#define TABLECODE___ ", table_fun),
          paste0("#define CONFIGFUN___ ", config_fun),
          paste0("#define _nEQ ", ncmt),
          paste0("#define _nPAR ", npar),
          cmtndef,
          Fdef,
          Adef,
          Rdef,
          Ddef,
          cmtdef,
          initdef,
          dxdef,
          pardef,
          etal,
          epsl
          )
        )
}


relocate_funs <- function(x,PACKAGE) {
    x@func[2] <- PACKAGE
    x@init_fun[2] <- PACKAGE
    x@table_fun[2] <- PACKAGE
    x@init_fun[2] <- PACKAGE
    x
}

## REMOVE 5/18/2016
# null_model <- function() {
#     x <- new("mrgmod", model="nullmodel")
# 
#     x@shlib$compiled <- TRUE
#     x@func <- c(ode_symbol("nullmodel"), "mrgsolve")
#     x@init_fun <- c("MRGSOLVE_NO_INIT_FUN", "mrgsolve")
#     x@table_fun <- c("MRGSOLVE_NO_TABLE_FUN", "mrgsolve")
#     as.locked(x,
#               file.path(path.package("mrgsolve"),"libs"),
#               dllname="mrgsolve",
#               src=file.path(path.package("mrgsolve"),"src"),
#               include=file.path(path.package("mrgsolve"),"include"))
# }
# 

as_PKMODEL <- function(x) {
    x@func <- c("MRGSOLVE_NO_ODE_FUN", "mrgsolve")
    x@init_fun <- c("MRGSOLVE_NO_INIT_FUN", "mrgsolve")
    x@table_fun <- c("MRGSOLVE_NO_TABLE_FUN", "mrgsolve")
    x@config_fun <- c("MRGSOLVE_NO_CONFIG_FUN", "mrgsolve")
    x <- new("packmod", x, package = "mrgsolve")
    x@shlib$compiled <- TRUE
    x@shlib$par <- pars(x)
    x@shlib$cmt <- cmt(x)
    return(x)
}



compiled_or_stop <- function(x) UseMethod("compiled_or_stop")
compiled_or_stop.mrgmod <- function(x) {
    if(compiled(x)) return(invisible(NULL))
    stop("Model may not have been successfully compiled.\n", "   Source: ", cfile(x),"\n   Shared object (DLL): ", sodll(x))
}

compiled <- function(x,status=NULL) {
    if(is.null(status)) return(x@shlib$compiled & dll_loaded(x))
    x@shlib$compiled <- status
    return(x)
}

clean_symbol <- function(x) {
    gsub("[[:punct:]]", "__", x)
}


setGeneric("main_symbol", function(x,...) standardGeneric("main_symbol"))
setGeneric("table_symbol", function(x,...) standardGeneric("table_symbol"))
setGeneric("ode_symbol", function(x,...) standardGeneric("ode_symbol"))
setGeneric("config_symbol", function(x,...) standardGeneric("config_symbol"))

setGeneric("model_symbols", function(x,...) standardGeneric("model_symbols"))
setMethod("main_symbol", "mrgmod", function(x,...) {main_symbol(model(x))})
setMethod("main_symbol", "character",  function(x,...) {paste0("_model_", clean_symbol(x), "_main__")})
setMethod("table_symbol", "mrgmod", function(x,...) {table_symbol(model(x))})
setMethod("table_symbol", "character",  function(x,...) {paste0("_model_", clean_symbol(x), "_table__")})
setMethod("ode_symbol", "mrgmod",       function(x,...) {ode_symbol(model(x))})
setMethod("ode_symbol","character",     function(x,...) {paste0("_model_",clean_symbol(x), "_ode__")})
setMethod("config_symbol", "mrgmod",    function(x,...) {config_symbol(model(x))})
setMethod("config_symbol","character",  function(x,...) {paste0("_model_",clean_symbol(x), "_config__")})

setMethod("model_symbols", "mrgmod",  function(x,...) {
    ans <- lapply(list(x@init_fun, x@func, x@table_fun,x@config_fun), function(i) {data.frame(i[1], i[2])})
    ans <- data.frame(do.call("rbind", ans))
    names(ans) <- c("symbol", "PACKAGE")
    ans
})

setGeneric("assign_symbols", function(x,...) standardGeneric("assign_symbols"))
setMethod("assign_symbols", "mrgmod", function(x,PACKAGE,model) {
    x@func <-      c(ode_symbol(x),dllname(x))
    x@init_fun <-  c(main_symbol(x),dllname(x))
    x@table_fun <- c(table_symbol(x),dllname(x))
    x@config_fun <- c(config_symbol(x),dllname(x))
    return(x)
})

# 
# config_function <- function(capture_names) {
#   n <- length(capture_names)
#   tot <- 5 + n
#   out <- vector("character", tot)
#   out[1] <- "void CONFIGFUN___(MRGSOLVE_CONFIG_SIGNATURE) {"
#   out[2] <- paste0("  unsigned int n = ", n, ";")
#   out[3] <- paste0("  capture_names.resize(",n,");")
#   out[4] <- paste0("  capture_values.assign(0.0,",n,");")
#   
#   nindex <- seq_along(capture_names)
#   out[nindex+4] <- paste0("  capture_names[",(nindex-1),"] = ", capture_names[nindex],";")
#   out[tot] <- "}"
#   return(out)
# }
# 
# 
