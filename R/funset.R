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


FUNSET_ERROR <- 
'
There was a problem accessing the model shared object.
  Either the model object is corrupted or  
  the model was not properly compiled.
Check mrgsolve:::funset(mod) for more information.
'

main_func   <- function(x) x@funs["main"]
ode_func    <- function(x) x@funs["ode"]
table_func  <- function(x) x@funs["table"] 
config_func <- function(x) x@funs["config"]
info_func   <- function(x) x@funs["info"]

main_loaded <- function(x) {
  is.loaded(main_func(x),PACKAGE=dllname(x)) 
}

clean_symbol <- function(x) {
  gsub("[[:punct:]]", "__", x)
}

funs_create <- function(model,what=c("main", "ode", "table", "config")) {
  setNames(paste0("_model_", clean_symbol(model), "_",what ,"__"),what)
}

register_fun <- function(model) {
  paste0("R_init_",model) 
}

package_loaded <- function(x) {
  is.element(dllname(x),names(getLoadedDLLs())) 
}

funs <- function(x) {
  x@funs[c("main", "ode", "table", "config")]
}

model_loaded <- function(x) {
  all(funs_loaded(x)) & package_loaded(x)
}

which_loaded <- function(x) {
  sapply(unname(funs(x)),is.loaded,PACKAGE=dllname(x),type="Call")
}

funs_loaded <- function(x,crump=TRUE) {
  all(which_loaded(x)) & compiled(x)
}

all_loaded <- function(x) all(which_loaded(x))  


pointers <- function(x) {
  if(!funs_loaded(x)) stop(FUNSET_ERROR)
  what <- funs(x)
  ans <- getNativeSymbolInfo(what,PACKAGE=dllname(x))
  setNames(lapply(ans, "[[","address"),names(what))
}

funset <- function(x) {
  pkg <- dllname(x)
  ans <- lapply(unname(funs(x)), function(w) {
      loaded <- is.loaded(w,pkg)
      if(loaded) {
        info <- getNativeSymbolInfo(w,pkg)
        name <- info$name
      } else {
        name <- as.character(NA)
      }
      dplyr::data_frame(name=name,loaded=loaded)
  }) 
  
  ans <- 
    dplyr::bind_rows(unname(ans)) %>% mutate(func = names(funs(x)))  %>%
    dplyr::select(func,name,loaded) %>% as.data.frame
  
  shlib <- dplyr::data_frame(package=pkg,
                             version=as.character(build_version(x)),
                             compiled=compiled(x)
                             )
  
  list(symbols = ans, shlib = data.frame(shlib))
}

