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


##' Simulate a sequence of parameters
##' 
##' This function is experimental and may change or go away at any time without
##' notice.
##' 
##' @param mod a model object
##' @param ... unquoted parameter names
##' @param n number of values to simulate
##' @param cv coefficient of variation; used to determine the minimum and 
##' maximum values for the sweep
##' @param nsd the number of standard deviations over which to sweep
##' 
##' @return A mrgsims object with a character columns named `.name`
##' indicating the parameter name and `.value` indicating the sweep
##' value.
##' 
##' @examples
##' mod <- mrgsolve:::house() %>% zero_re()
##' 
##' mod %>% 
##'   ev(amt = 100) %>% 
##'   wf_sweep(CL, VC) %>% 
##'   plot(CP~time|.name)
##' 
##' @keywords internal
##' @md
##' @export
wf_sweep <- function(mod, ..., n=10, cv=30, nsd = 2) {
  par <- dplyr::select(as_tibble(as.list(param(mod))),...)
  point <- as.list(par)
  out <- vector(mode = "list", length = length(point))
  values <- vector(mode = "list", length = length(point))
  std <- sqrt((cv/100)^2)
  .n <- n
  for(p in seq_along(point)) {
    x <- point[[p]]
    if(x <= 0) {
      stop(
        "All sweep values must be >= 0 (",
        names(point)[p], 
        ").",
        call.=FALSE
      )
    }
    from <- log(x) - nsd*std
    to <-   log(x) + nsd*std
    valu <- exp(seq(from, to, length.out = n))    
    values[[p]] <- valu
    points <- point
    points[[p]] <- valu
    out[[p]] <- expand.grid(points)
  }
  out <- bind_rows(out)
  out <- mutate(out, ID = seq(n()))
  sims <- mrgsim(mod,idata=out)
  ntot <- nrow(sims)
  sims <- mutate_sims(
    sims, 
    .name = rep(names(point), each = ntot/length(point)), 
    .value = rep(signif(unlist(values),3), each = ntot/length(point)/.n)
  )
  sims
}
