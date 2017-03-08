# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
#
# This file is part of mrgsolve.
#
# mrgsolve is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsolve is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.


print_leading <- "%-17s"
parheader <- c("  Parameters: ", rep("  >", 6))
initheader <- gsub("Parameters", "Compartments", parheader)
parheader <- sprintf(print_leading, parheader)
initheader <- sprintf(print_leading, initheader)
timeheader <- sprintf(print_leading,c("  Time: ", "  >", "  >"))
print.os.header <- sprintf(print_leading, "  OS type: ")
print.solver.header <- sprintf(print_leading, c("  Solver:", "  >"))


print.mrgmod <- function(x,verbose=FALSE,...) {

    ##Sys.setlocale("LC_COLLATE","C")

    add <- x@add
    nadd <- length(add)

    tt1 <- c(start=x@start,end=x@end, delta=x@delta)
    tt1 <- paste(names(tt1), tt1, sep=": ")
    tt1 <- paste(tt1, collapse=" ")

    add_suffix <- ""
    if(nadd > 7) {
        add <- add[1:7]
        add_suffix <- " ..."
    }

    add <- paste0("add: " , paste(add, collapse=" "),add_suffix)

    if(nadd ==0) add <- "add: <none>"
    tt <- c(tt1,add, paste0("tscale: ", x@tscale))
    tt <- paste0(timeheader,tt)


    pars <- param(x)
    npars <- length(pars)

    if(npars > 0) {
        partxt <- split(names(pars), ceiling(seq_along(pars)/6))

        if(length(partxt) > 4) {
            partxt <- partxt[1:4]
            partxt[[5]] <- "..."
        }

        partxt[[length(partxt)]] <- c(partxt[[length(partxt)]], paste0("[", npars,"]"))
        partxt <- sapply(partxt, function(i) paste(i, collapse=" "))

        partxt <- paste0(parheader[1:length(partxt)],partxt)
    } else {
        partxt <- paste0(parheader[1],"<none>")
    }

    inits <- cmt(x)
    ninit <- length(inits)
    if(ninit > 0) {

        inittxt <- split(inits, ceiling(seq_along(inits)/6))
        if(length(inittxt)>4) {
            inittxt <- inittxt[1:4]
            inittxt[[5]] <- "..."
        }
        inittxt[[length(inittxt)]] <- c(inittxt[[length(inittxt)]], paste0("[",ninit,"]"))
        inittxt <- sapply(inittxt,function(i) paste(i, collapse=" "))
        inittxt <- paste0(initheader[1:length(inittxt)],inittxt)
    } else {
        inittxt <- paste0(initheader[1],"<none>")
    }
    solvertxt <- list(atol=x@atol, rtol=x@rtol,maxsteps=x@maxsteps, hmin=x@hmin, hmax=x@hmax)
    solvertxt <- paste(names(solvertxt), unlist(solvertxt), sep=": ")
    solvertxt <- list(solvertxt[1:2], solvertxt[3:5])
    solvertxt <- sapply(solvertxt, function(i) paste(i, collapse= " "))
    solvertxt <- paste0(print.solver.header, solvertxt)

    evtxt <- "<none>"
    ev <- as.data.frame(events(x))
    if(nrow(ev)>0) evtxt <- paste0("Yes (", nrow(ev), " rows)")

    proj <- normalizePath(project(x), mustWork=FALSE,winslash=.Platform$file.sep)
    proj <- cropstr(proj, 16,44)


    osig <- dim_matlist(x@omega)
    osig <- paste(osig,osig, sep="x", collapse=',')
    ssig <- dim_matlist(x@sigma)
    ssig <- paste(ssig,ssig, sep="x", collapse=',')

    loaded <- ifelse(model_loaded(x),"", "<not loaded>")

    header <- paste0("\n\n-------- mrgsolve model object (", .Platform$OS.type, ") --------\n")
    cat(header)
    cat("  Project: ",proj,"\n", sep="")
    cat("  source:        ", basename(cfile(x)), "\n", sep="")
    cat("  shared object: ", dllname(x), " ",loaded,"\n\n", sep="")
    cat("  compile date:  ", x@shlib$date, "\n", sep="")


    cat(tt, sep="\n")
    cat("\n")

    cat(inittxt, sep="\n")
    cat(partxt, sep="\n")
    cat("  Omega:        ", osig, "\n")
    cat("  Sigma:        ", ssig, "\n")

    cat("\n")


    cat(solvertxt, sep="\n")

    cat("\n")

}

##' Print model details.
##'
##' @param object the model object
##' @export
setMethod("show", "mrgmod", function(object) print.mrgmod(object))



