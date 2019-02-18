# Copyright (C) 2013 - 2019  Metrum Research Group, LLC
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
parheader <- c("  parameters: ", rep("    ", 6))
initheader <- gsub("parameters", "compartments", parheader)
captheader <- gsub("parameters", "captures", parheader)
parheader <- sprintf(print_leading, parheader)
initheader <- sprintf(print_leading, initheader)
captheader <- sprintf(print_leading, captheader)
timeheader <- sprintf(print_leading,c("  time: ", "    "))
print.os.header <- sprintf(print_leading, "  OS type: ")
print.solver.header <- sprintf(print_leading, c("  solver:"))


print.mrgmod <- function(x,verbose=FALSE,...) {

  add <- x@add
  nadd <- length(add)
  
  tt1 <- c(start=x@start,end=x@end, delta=x@delta)
  tt1 <- paste(names(tt1), tt1, sep=": ")
  tt1 <- paste(tt1, collapse=" ")
  
  add_suffix <- ""
  add <- paste0(add,collapse=" ")
  add <- strwrap(add, width=30)
  if(length(add) > 1) {
    add <- add[1]
    add_suffix <- " ..."
  }
  
  add <- paste0("add: " , paste0(add,add_suffix))
  
  if(nadd ==0) add <- "add: <none>"
  tt <- c(tt1,add)
  tt <- paste0(timeheader,tt)
  
  
  pars <- param(x)
  npars <- length(pars)
  
  if(npars > 0) {
    partxt <- strwrap(paste0(names(pars),collapse=" "),width = 40)
    if(length(partxt) > 4) {
      partxt <- partxt[1:4]
      partxt[5] <- "..."
    }
    partxt[length(partxt)] <- paste0(partxt[length(partxt)], " [", npars,"]")
    partxt <- paste0(parheader[1:length(partxt)],partxt)
  } else {
    partxt <- paste0(parheader[1],"<none>")
  }
  
  inits <- cmt(x)
  ninit <- length(inits)
  if(ninit > 0) {
    inittxt <- strwrap(paste0(inits,collapse=" "),width = 40)
    if(length(inittxt) > 4) {
      inittxt <- inittxt[1:4]
      inittxt[5] <- "..."
    }
    inittxt[length(inittxt)] <- paste0(inittxt[length(inittxt)], " [", ninit,"]")
    inittxt <- paste0(initheader[1:length(inittxt)],inittxt)
  } else {
    inittxt <- paste0(initheader[1],"<none>")
  }
  
  capt <- x@capture
  ncapt <- length(capt)
  if(ncapt > 0) {
    capttext <- strwrap(paste0(capt,collapse=" "),width = 40)
    if(length(capttext) > 4) {
      capttext <- capttext[1:4]
      capttext[5] <- "..."
    }
    capttext[length(capttext)] <- paste0(capttext[length(capttext)], " [", ncapt,"]")
    capttext <- paste0(captheader[1:length(capttext)],capttext)
    
  } else {
    capttext <- paste(captheader[1],"<none>")  
  }
  
  
  solvertxt <- list(atol=x@atol,rtol=x@rtol,maxsteps=x@maxsteps)
  solvertxt <- paste(names(solvertxt), unlist(solvertxt), sep=": ")
  solvertxt <- list(solvertxt[1:3])
  solvertxt <- sapply(solvertxt, function(i) paste(i, collapse= " "))
  solvertxt <- paste0(print.solver.header, solvertxt)
  
  proj <- normalizePath(project(x), mustWork=FALSE,
                        winslash=.Platform$file.sep)
  proj <- cropstr(proj, 16,16)
  
  
  osig <- dim_matlist(x@omega)
  osig <- paste(osig,osig, sep="x", collapse=',')
  ssig <- dim_matlist(x@sigma)
  ssig <- paste(ssig,ssig, sep="x", collapse=',')
  
  loaded <- ifelse(model_loaded(x),"", "<not loaded>")
  
  src <- paste0("source: ", basename(cfile(x)))
  nsrc <- nchar(src)
  nside <- (52-nsrc)/2 - 2
  side <- paste0(rep("-", nside),collapse="")

  header <- paste0("\n\n",side, "  ", src, "  ", side, "\n\n")
  
  cat(header)
  
  cat("  project: ",proj,"\n", sep="")
  cat("  shared object: ", dllname(x), " ",loaded,"\n\n", sep="")

  cat(tt, sep="\n")
  cat("\n")
  
  cat(inittxt, sep="\n")
  cat(partxt, sep="\n")
  cat(capttext,sep="\n")
  cat("  omega:        ", osig, "\n")
  cat("  sigma:        ", ssig, "\n")
  
  cat("\n")
  
  
  cat(solvertxt, sep="\n")
  
  cat("\n")
  
}

##' Print model details
##'
##' @param object the model object
##' @export
##' @keywords internal
setMethod("show", "mrgmod", function(object) print.mrgmod(object))



