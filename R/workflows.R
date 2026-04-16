# Copyright (C) 2013 - 2026  Metrum Research Group
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


##' DEPRECATED: Simulate a sequence of parameters
##' 
##' This function is experimental and may change or go away at any time without
##' notice.
##' 
##' 
##' @return nothing; an error is generated if this is called.
##' 
##' @examples
##' \dontrun{
##' try(wf_sweep())
##' }
##' @keywords internal
##' @md
##' @export
wf_sweep <- function(...) {
  abort("`wf_sweep()` is deprecated; try the 'mrgsim.sa' package instead.")
}
