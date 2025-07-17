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

##' @rdname matlist-class
##' @export
##' @keywords internal
setClass("omegalist", contains="matlist")

##' @rdname matlist-class
##' @export
##' @keywords internal
setClass("sigmalist", contains="matlist")

##' S4 parameter_list class
##' @keywords internal
setClass("parameter_list",contains="numericlist")

##' S4 cmt_list class
##' @keywords internal
setClass("cmt_list",contains="numericlist")
