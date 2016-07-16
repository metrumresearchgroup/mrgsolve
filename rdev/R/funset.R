##' @include classes.R
##' 
##' 
##' 
##' 


main_func   <- function(x) x@funs["main"]
table_func  <- function(x) x@funs["table"]
ode_func    <- function(x) x@funs["ode"]
config_func <- function(x) x@funs["config"]


clean_symbol <- function(x) {
  gsub("[[:punct:]]", "__", x)
}

funs_create <- function(model,what=c("main", "ode", "table", "config")) {
  setNames(paste0("_model_", clean_symbol(model), "_",what ,"__"),what)
}


package_loaded <- function(x) {
  is.element(package(x),names(getLoadedDLLs())) 
}

funs <- function(x) {
  x@funs[c("main", "ode", "table", "config")]
}

model_loaded <- function(x) {
  if(!package_loaded(x)) stop("The model (so/dll): ", package(x) , " is not loaded.")
  all(funs_loaded(x)) & package_loaded(x)
}

which_loaded <- function(x) {
  pkg <- package(x)
  if(!package_loaded(x)) stop("The model (dll/package): ", package(x), " is not loaded.")
  sapply(unname(funs(x)),is.loaded,PACKAGE=pkg,type="Call")
}

funs_loaded <- function(x,crump=TRUE) {
  all(which_loaded(x))
}

pointers <- function(x) {
  if(!funs_loaded(x)) stop("Can't get function pointers ... some functions are not loaded.")
  ans <- getNativeSymbolInfo(funs(x),PACKAGE=package(x))
  ans <- lapply(ans, function(xx) xx$address)
  setNames(ans, c("init", "deriv", "table", "config"))
}

show_funset <- function(x) {
  pkg <- package(x)
  ans <- lapply(unname(funs(x)), function(w) {
      loaded <- is.loaded(w,pkg)
      if(loaded) {
        info <- getNativeSymbolInfo(w,pkg)
        address <- capture.output(print(info$address))[1]
        name <- info$name
      } else {
        address <- as.character(NA)
        name <- as.character(NA)
      }
      dplyr::data_frame(name=name,address=address,loaded=loaded)
  }) 
  
  ans <- 
    dplyr::bind_rows(unname(ans)) %>% mutate(func = names(funs(x)))  %>%
    dplyr::select(func,name,address,loaded) %>% as.data.frame
  
  list(symbols = ans ,package=pkg)
}

