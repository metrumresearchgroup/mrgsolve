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


##' Example input data sets.
##'
##' @docType data
##' @keywords datasets
##' @rdname exdatasets
##' @name exdatasets
##' @details
##' 
##' \itemize{
##'  \item \code{exidata} holds individual-level parameters and other data items, one per row
##'  \item \code{extran1} is a "condensed" data set
##'  \item \code{extran2} is a full dataset
##'  \item \code{extran3} is a full dataset with parameters
##'  \item \code{exTheoph} is the theophylline data set, ready for input into \code{mrgsolve}
##'  \item \code{exBoot} a set of bootstrap parameter estimates
##'
##' }
##'
##' @examples
##'
##' mod <- mrgsolve:::house() %>% update(end=240) %>% Req(CP)
##'
##' ## Full data set
##' data(exTheoph)
##' out <- mod %>% data_set(exTheoph) %>% mrgsim
##' out
##' plot(out)
##' 
##' ## Condensed: mrgsolve fills in the observations
##' data(extran1)
##' out <- mod %>% data_set(extran1) %>% mrgsim
##' out
##' plot(out)
##' 
##' ## Add a parameter to the data set
##' stopifnot(require(dplyr))
##' data <- extran1 %>% distinct(ID) %>% select(ID) %>%
##'   mutate(CL=exp(log(1.5) + rnorm(nrow(.), 0,sqrt(0.1)))) %>%
##'   left_join(extran1,.)
##'   
##' data
##' 
##' out <- mod %>% data_set(data) %>% carry.out(CL) %>%  mrgsim
##' out
##' plot(out)
##' 
##' ## idata
##' data(exidata)
##' out <- mod %>% idata_set(exidata) %>% ev(amt=100,ii=24,addl=10) %>% mrgsim
##' plot(out, CP~time|ID)
NULL

##' @docType data
##' @keywords datasets
##' @rdname exdatasets
##' @usage data(exidata)
##' @name exidata
NULL

#' @docType data
#' @keywords datasets
#' @name extran1
#' @usage data(extran1)
#' @rdname exdatasets
NULL

#' @docType data
#' @keywords datasets
#' @name extran2
#' @usage data(extran2)
#' @rdname exdatasets
NULL

#' @docType data
#' @keywords datasets
#' @name extran3
#' @usage data(extran3)
#' @rdname exdatasets
#'
NULL

#' @docType data
#' @keywords datasets
#' @name exTheoph
#' @usage data(exTheoph)
#' @rdname exdatasets
NULL

#' @docType data
#' @keywords datasets
#' @name exBoot
#' @usage data(exBoot)
#' @rdname exdatasets
NULL
