## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


##' @include utils.R

## variables related to RDEFS:


CODE_ARCHIVE <- "
#define NO_init_fun BEGIN_init_fun END_init_fun
#define NO_table BEGIN_table END_table
extern \"C\"{void MRGSOLVE_NO_INITS_FUN(MRGSOLVE_INIT_SIGNATURE) {};};
extern \"C\"{void MRGSOLVE_NO_TABLES_FUN(MRGSOLVE_TABLE_SIGNATURE) {};};
extern \"C\"{int MRGSOLVE_GET_NPARM() {return MRGSOLVE_NPARAM_;};};
extern \"C\"{int MRGSOLVE_GET_NEQ() {return MRGSOLVE_NEQ_;};};

"


write.rdefs <- function(x,def.file=paste(dllname(x),"h",sep="."),...) {


    modelheader <-file.path(system.file(package="mrgsolve"), "include", "modelheader.h")


    #message("Writing R definitions to C header file...")
    stopifnot(valid_project(x))
    def.file <- file.path(project(x),def.file)

    foo <- file.copy(modelheader, project(x), overwrite=TRUE)
    if(!foo) stop("Could not copy header file out of R lib directory")

    pars <- names(param(x))
    cmt <-  cmt(x)

    omats <- cumoffset(omat(x))
    smats <- cumoffset(smat(x))

    txt <- generate_rdefs(pars,cmt,ode_symbol(x), main_symbol(x), table_symbol(x),model(x),omats,smats,...)

    def.con <- file(def.file, open="w")
    cat(txt, sep="\n", file=def.con)
    close(def.con)

    x@shlib$cmt <- cmt
    x@shlib$par <- pars

    return(x)
}




generate_rdefs <- function(pars,
                           cmt,
                           func,
                           init_fun="MRGSOLVE_NO_INIT_FUN",
                           table_fun="MRGSOLVE_NO_TABLE_FUN",
                           model="",omats,smats,parsedata=list(),...) {

    npar <- length(pars)
    ncmt <- length(cmt)

    dxdt <- paste0("dxdt_",cmt)
    init <- paste0(cmt, "_0")

    cmtindex <- (1:length(cmt))-1
    parsindex <- (1:length(pars))-1

    cmtdef <-  paste("#define ", cmt,  " _A_[",cmtindex,"]", sep="",collapse="\n")
    initdef <- paste("#define ", init,  " _A_0_[",cmtindex,"]", sep="",collapse="\n")
    dxdef <-   paste("#define ", dxdt, " _DADT_[",cmtindex,"]", sep="",collapse="\n")

    pardef <-  paste("#define ", pars, " _THETA_[",parsindex,"]", sep="",collapse="\n")

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
        cmtndef <- paste("#define ", paste0("N_", cmtn), " ", cmtnindex, sep="", collapse="\n")
        Fdef <- paste("#define ", paste0("F_",cmtn), " _F_[",cmtnindex-1,"]", sep="",collapse="\n")
        Adef <- paste("#define ", paste0("ALAG_",cmtn), " _ALAG_[",cmtnindex-1,"]", sep="",collapse="\n")
        Ddef <- paste("#define ", paste0("D_",cmtn), " _D_[",cmtnindex-1,"]", sep="",collapse="\n")
        Rdef <- paste("#define ", paste0("R_",cmtn), " _R_[",cmtnindex-1,"]", sep="",collapse="\n")

    }

    if(npar==0) pardef <- ""
    if(ncmt==0) {
        cmtdef <- initdef <- dxdef <- ""
    }

    lines <- c(
        "//* MRGSOLVE file" ,
        "#include \"modelheader.h\"",
        "#ifndef MODELINCLUDEGUARD",
        paste0("#define INITFUN___ ",init_fun),
        paste0("#define ODEFUN___ ",func),
        paste0("#define TABLECODE___ ", table_fun),
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
        epsl,
        "#define MODELINCLUDEGUARD",
        "#endif"
        )
    return(lines)
}



## setGeneric("compile", function(x,...) standardGeneric("compile"))
## setMethod("compile", "mrgmod", function(x,rdefs=TRUE,preclean=FALSE,unload.first=TRUE,verbose.shlib=FALSE,...) {

##     if(!file.exists(cfile(x))) stop("Could not find model file: ", cfile(x))

##     x <- compiled(x,FALSE)

##     if(unload.first | .Platform$OS.type!="unix") try(dyn.unload(sodll(x)),silent=TRUE)

##     preclean <- x@preclean | preclean

##     x <- write.rdefs(x,...)

##     if(header_warning(x)) {
##         message("Header file has changed; forcing preclean = TRUE")
##         preclean <- TRUE
##     }

##     system.cmd <- paste("R CMD SHLIB  ", ifelse(preclean,"--preclean",""),cfile(x))

##     skip <- checks_same(x)

##     if(!skip | preclean) {
##         message("Model: ", cfile(x))
##         message("Compiling... ", appendLF=FALSE)
##     }

##     if(skip & !preclean & !x@verbose) message("Compile: nothing to be done.")

##     status <- system(system.cmd, ignore.stdout = !verbose.shlib)

##     if(status!=0) {
##         message("Error while compiling model ", cfile(x))
##         x <- compiled(x,FALSE)
##     } else {
##         if(!skip) message("done.")
##         dyn.load(sodll(x))
##         if(!dll_loaded(x)) stop("Model was not found after attempted loading.")
##         x <- compiled(x,TRUE)
##         write_check_file(x)
##     }

##     x@shlib$date <- date()
##     x@shlib$version <- VERSION

##     return(x)
## })


## check_file_name <- function(x, ...) {
##     ## Generate the name of the check file
##     filename(project(x),dllname(x), ".check")
## }
## write_check_file <- function(x,data=NULL) {
##     if(is.null(data)) data <- check_file(x)
##     attr(data, "date") <- date()
##     dput(file=check_file_name(x),data)
## }
## read_check_file <- function(x,file=check_file_name(x)) {
##     if(!file.exists(file)) return(list(h="nofiletoread", c="nofiletoread"))
##     dget(file=file)
## }
## check_file <- function(x) {
##     check <- tools::md5sum(c(path.expand(hfile(x)),path.expand(cfile(x))))
##     names(check) <- c("h", "c")
##     check[is.na(check)] <- "nofiletocheck"
##     check
## }
## checks_same <- function(x) {

##     all(read_check_file(x)==check_file(x))
## }
## header_same <- function(x) {
##     read_check_file(x)["h"] == check_file(x)["h"]
## }
## check_file_exists <- function(x) {
##     file.exists(check_file_name(x))
## }
## header_warning <- function(x) {
##     check <- check_file(x)
##     read <- read_check_file(x)
##     (check["h"] != read["h"]) & check_file_exists(x) & check["c"] == read["c"]
## }

relocate_funs <- function(x,PACKAGE) {
    x@func[2] <- PACKAGE
    x@init_fun[2] <- PACKAGE
    x@table_fun[2] <- PACKAGE
    x
}

null_model <- function() {
    x <- new("mrgmod", model="nullmodel")

    x@shlib$compiled <- TRUE
    x@func <- c(ode_symbol("nullmodel"), "mrgsolve")
    x@init_fun <- c("MRGSOLVE_NO_INIT_FUN", "mrgsolve")
    x@table_fun <- c("MRGSOLVE_NO_TABLE_FUN", "mrgsolve")
    as.locked(x,
              file.path(path.package("mrgsolve"),"libs"),
              dllname="mrgsolve",
              src=file.path(path.package("mrgsolve"),"src"),
              include=file.path(path.package("mrgsolve"),"include"))
}


as_PKMODEL <- function(x) {
    x@func <- c("MRGSOLVE_NO_ODE_FUN", "mrgsolve")
    x@init_fun <- c("MRGSOLVE_NO_INIT_FUN", "mrgsolve")
    x@table_fun <- c("MRGSOLVE_NO_TABLE_FUN", "mrgsolve")
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

setGeneric("model_symbols", function(x,...) standardGeneric("model_symbols"))
setMethod("main_symbol", "mrgmod", function(x,...) {main_symbol(model(x))})
setMethod("main_symbol", "character",  function(x,...) {paste0("_model_", clean_symbol(x), "_main__")})
setMethod("table_symbol", "mrgmod", function(x,...) {table_symbol(model(x))})
setMethod("table_symbol", "character",  function(x,...) {paste0("_model_", clean_symbol(x), "_table__")})
setMethod("ode_symbol", "mrgmod",  function(x,...) {ode_symbol(model(x))})
setMethod("ode_symbol","character",  function(x,...) {paste0("_model_",clean_symbol(x), "_ode__")})
setMethod("model_symbols", "mrgmod",  function(x,...) {
    ans <- lapply(list(x@init_fun, x@func, x@table_fun), function(i) {data.frame(i[1], i[2])})
    ans <- data.frame(do.call("rbind", ans))
    names(ans) <- c("symbol", "PACKAGE")
    ans
})

setGeneric("assign_symbols", function(x,...) standardGeneric("assign_symbols"))
setMethod("assign_symbols", "mrgmod", function(x,PACKAGE,model) {
    x@func <-      c(ode_symbol(x),dllname(x))
    x@init_fun <-  c(main_symbol(x),dllname(x))
    x@table_fun <- c(table_symbol(x),dllname(x))
    return(x)
})



