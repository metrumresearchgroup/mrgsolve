## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


shdate <- function(x) format(as.POSIXct(x, origin="1970-01-01"), "%m/%d %H:%M")
shtime <- function(x) format(as.POSIXct(x,origin="1970-01-01"), "%H:%M:%S")
ntime <- function(x) as.numeric(Sys.time())
SAFE_WAIT_TIME <- 2

complog1 <- dplyr::data_frame(md5=" ",
                              time=0,
                              file=" ",
                              so=" ",
                              project=" ")

complog0 <- complog1[0,]

## Assign the complog to the db environment
comp_assign <- function(x) assign("complog",x,envir=db)

## Store a model in the complog
store <- function(x,purge=TRUE) {
  
  data <- dplyr::data_frame(md5     = tools::md5sum(cfile(x)),
                            time    = ntime(),
                            file    = basename(cfile(x)),
                            so      = sodll(x),
                            model   = model(x),
                            project = project(x))
  
  db[["complog"]] %>%
    dplyr::bind_rows(data) %>%
    comp_assign
  
  return(invisible(NULL))
}

## number of compiled models
n_comp <- function() db[["complog"]] %>% nrow



##' Functions for viewing and manipulating the compilation log.
##'
##' \code{complog} displays all of the models in the compilation log.  \code{comp_forget} removes models from the compilation log and attempts to unload the corresponding shared object.
##'
##' @param full show a full display
##' @export
complog <- function(full=FALSE) {
  
  if(full) return(db[["complog"]])
  
  if(n_comp()==0) {
    message("No models found.")
    return(invisible(NULL))
  }
  
  so <- md5 <- NULL
  
  db[["complog"]] %>%
    dplyr::select_(.dots=c("time","so","md5","file")) %>%
    dplyr::mutate(so = basename(so),time=shtime(time)) %>%
    dplyr::mutate(md5=cropstr(md5, 8,6)) %>%
    as.data.frame
}

## Drop a model from the comp log
## This also removes the shared object
drop_model <- function(x) {
  so <-
    x %>%
    dplyr::select(so) %>%
    dplyr::distinct(so) %>%
    unlist
  
  for(i in so) try(dyn.unload(i),silent=TRUE)
  
  x <- suppressWarnings(file.remove(so[file.exists(so)]))
  
  if(any(!x)) warning("Could not delete model shared object.")
  
  return(invisible(NULL))
  
}

check_and_copy <- function(from,to,preclean=FALSE) {
  
  if(!file.exists(to)) {
    file.copy(from,to)
    same <- TRUE
  } else {
    same <- tools::md5sum(from) == tools::md5sum(to)
    if((!same) | preclean) {
      file.copy(from,to,overwrite=TRUE)
    }
  }

  z <- file.remove(from)
  
  return(same)
}

## Wait a certain amount of time before re-compiling
## and loading a model
safe_wait <- function(x) {
  
  target <- compout(model(x),soloc(x))
  if(!file.exists(target)) return(invisible(NULL))
  mt <- file.info(target)[["mtime"]]
  age <- as.POSIXct(Sys.time()) - as.POSIXct(mt)
  if(age > SAFE_WAIT_TIME) return(invisible(NULL))
  message("(waiting) ...")
  return(Sys.sleep(SAFE_WAIT_TIME-age))
}

## Drop a model from the log
purge_model <- function(.file) {
  x <- db[["complog"]]
  what <- file.path(x$project,x$file) ==.file
  x %>% filter(what) %>% drop_model
  x %>% filter(!what) %>%  comp_assign
  return(invisible(NULL))
}

## Is a model in the log?
logged <- function(x) {
  nrow(db[["complog"]] %>% filter(model==x)) > 0
}



grab_file <- function(x) {
  
  x <- 
    db[["complog"]] %>%
    filter(file==basename(x)) %>%
    slice(dplyr::n())
  
  if(nrow(x)==0) return(complog1)
  
  return(x)
}

##' @export
##' @rdname complog
##' @param x not used
comp_forget <- function(x) {
  
  ## We're forgetting every model here
  db[["complog"]] %>% drop_model
  
  y <- 
    db[["complog"]] %>% 
    distinct(project) %>% 
    select(project) %>% 
    unlist
  
  o <- list.files(y,pattern="*\\.o$",full.names=TRUE)
  
  cppcpp <- list.files(y,pattern="*\\__cpp\\.cpp",full.names=TRUE)
  
  unlink(o)
  
  unlink(cppcpp)
  
  complog0 %>% comp_assign
  
  return(invisible(NULL))
}

## This is the environment for the complog data base
db <- new.env()
db$complog <- complog0
db$last <-0




