## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


##' @include utils.R


generate_rdefs <- function(pars,
                           cmt,
                           func,
                           init_fun="",
                           table_fun="",
                           config_fun="",
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
    x@package <- PACKAGE
    x
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
