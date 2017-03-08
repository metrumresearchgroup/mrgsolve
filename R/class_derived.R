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

##' @export
##' @rdname matlist-class
setClass("omegalist", contains="matlist")

##' @export
##' @rdname matlist-class
setClass("sigmalist", contains="matlist")

##' S4 parameter_list class
##' @details
##' parameter_list is a \code{\link{numericlist-class}}
setClass("parameter_list",contains="numericlist")

##' S4 cmt_list class
##' @details
##' cmt_list is a \code{\link{numericlist-class}}
setClass("cmt_list",contains="numericlist")


