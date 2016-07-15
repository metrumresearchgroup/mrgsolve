##' @include classes.R
##' 
##' 
##' 
##' 


main_func   <- function(x) x@funs["main"]
table_func  <- function(x) x@funs["table"]
ode_func    <- function(x) x@funs["ode"]
config_func <- function(x) x@funs["config"]
package_func <- function(x) x@funs["PACKAGE"]

clean_symbol <- function(x) {
  gsub("[[:punct:]]", "__", x)
}

funs_create <- function(model,package,what=c("main", "ode", "table", "config")) {
  c(PACKAGE=package,setNames(paste0("_model_", clean_symbol(model), "_",what ,"__"),what))
}


package_loaded <- function(x) {
  is.element(x["PACKAGE"],names(getLoadedDLLs())) 
}

funs_all <- function(x) {
  x[c("main", "table", "ode", "config")]
}

model_loaded <- function(x) {
  if(!package_loaded(x@funs)) stop("The model (so/dll): ", package_func(x) , " is not loaded.")
  all(funs_loaded(x@funs)) & package_loaded(x@funs)
}

which_loaded <- function(x) {
  if(!package_loaded(x["PACKAGE"])) stop("The model (dll/package): ", x["PACKAGE"], " is not loaded.")
  what <- funs_all(x)
  sapply(unname(what),is.loaded,PACKAGE=x["PACKAGE"],type="Call")
}

funs_loaded <- function(x,crump=TRUE) {
  all(which_loaded(x))
}

pointers <- function(x) {
  if(!funs_loaded(x)) stop("Can't get function pointers ... some functions are not loaded.")
  ans <- getNativeSymbolInfo(c(x["main"],x["ode"],x["table"],x["config"]),PACKAGE=x["PACKAGE"])
  ans <- lapply(ans, function(xx) xx$address)
  setNames(ans, c("init", "deriv", "table", "config"))
}

show_funset <- function(x) {
  
  what <- funs_all(x)
  
  ans <- lapply(unname(what), function(w) {
      loaded <- is.loaded(w,x["PACKAGE"])
      if(loaded) {
        info <- getNativeSymbolInfo(w,x["PACKAGE"])
        address <- capture.output(print(info$address))[1]
        name <- info$name
      } else {
        address <- as.character(NA)
        name <- as.character(NA)
      }
      dplyr::data_frame(name=name,address=address,loaded=loaded)
  }) 
  
  ans <- 
    dplyr::bind_rows(unname(ans)) %>% mutate(func = names(what))  %>%
    dplyr::select(func,name,address,loaded) %>% as.data.frame
  
  list(symbols = ans ,package=x["PACKAGE"])
}

