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

make_altnames <- function(from,to) {
  if(missing(to)) to <- from
  paste(to,from,sep="=")
}

set_altname <- function(x) {
  if(length(x)==0) {
    return(structure(list(from="",to="",rename=FALSE),class="altname"))
  }
  y <- strsplit(as.character(x),"\\s*=\\s*")
  to <- sapply(y,`[`,1L)
  from <- sapply(y,`[`,2L)
  from <- ifelse(is.na(from), to, from)
  if(identical(from,to)) return(as.character(from))
  return(structure(list(from=from,to=to,rename=!identical(from,to)),class="altname"))
  
}

altname <- function(x,...) UseMethod("altname")
altname.default <- function(x,y,...) return(y)
altname.altname <- function(x,y, ...) {
  old <- match(y,x[["from"]])
  old <- sort(old[!is.na(old)])
  nw <- match(x[["from"]],y)
  nw <- nw[!is.na(nw)]
  y[nw] <- x[["to"]][old]
  return(y)
}

as.character.altname <- function(x,...) {
  as.character(x[["from"]])
}




