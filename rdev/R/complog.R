## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


shdate <- function(x) format(as.POSIXct(x, origin="1970-01-01"), "%m/%d %H:%M")
shtime <- function(x) format(as.POSIXct(x,origin="1970-01-01"), "%H:%M:%S")
ntime <- function(x) as.numeric(Sys.time())
SAFE_WAIT_TIME <- 2

complog1 <- data.frame(md5=" ",
                      time=0,
                      file=" ",
                      so=" ",
                      project=" ",
                      stringsAsFactors=FALSE)
complog0 <- complog1[0,]



comp_assign <- function(x) assign("complog",x,envir=db)

store <- function(x,purge=TRUE) {

    data <- dplyr::data_frame(md5 = tools::md5sum(cfile(x)),
                              time=ntime(),
                              file=basename(cfile(x)),
                              so=sodll(x),
                              model = model(x),
                              project=project(x))

    db[["complog"]] %>%
        bind_rows(data) %>%
            comp_assign

  return(invisible(NULL))
}

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


drop_model <- function(x) {
  so <- x %>%
    dplyr::select(so) %>%
    dplyr::distinct(so) %>%
    unlist
  for(i in so) try(dyn.unload(i),silent=TRUE)
  x <- suppressWarnings(file.remove(so[file.exists(so)]))
  if(any(!x)) warning("Could not delete model shared object.")
  return(invisible(NULL))
}


## model_changed <- function(x,temp) {
##     y <- temp
##     z <- compfile(model(x),project(x))
##     a <- tools::md5sum(temp)
##     b <- tools::md5sum(z)
##     list(a=a,b=b,status=a==b,y=y,z=z)
## }

check_and_copy <- function(x,temp,preclean=FALSE) {

    from <- compfile(temp,project(x))
    to <- compfile(model(x),project(x))

    if(!file.exists(to)) {
        file.copy(from,to)
    } else {
        same <- tools::md5sum(from) == tools::md5sum(to)
        if((!same) | preclean) {
            file.copy(from,to,overwrite=TRUE)
        }
    }
    return(invisible(NULL))
}

## file.age <- function(x) {
##   x <- file.mtime(x) %>% as.numeric
##   y <- Sys.time() %>% as.numeric
##   return((y-x)/60)
## }


safe_wait <- function(x) {

    y <- db[["complog"]] %>%
        filter(file==basename(cfile(x))) %>%
            select_(.dots="time") %>% unlist

    if(length(y) ==0) return(invisible(NULL))

    z <- signif(ntime() - max(y),3)

    if(z > SAFE_WAIT_TIME) return(invisible(NULL))

    message("(waiting) ... ",appendLF=FALSE)
    return(Sys.sleep(SAFE_WAIT_TIME-z))
}

purge_model <- function(.file) {
    x <- db[["complog"]]
    what <- file.path(x$project,x$file) ==.file
    x %>% filter(what) %>% drop_model
    x %>% filter(!what) %>%  comp_assign
    return(invisible(NULL))
}

logged <- function(x) {
    nrow(db[["complog"]] %>% filter(model==x)) > 0
}



grab_file <- function(x) {

  x <- db[["complog"]] %>%
    filter(file==basename(x)) %>%
        slice(dplyr::n())

  if(nrow(x)==0) return(complog1)
  return(x)
}

##' @export
##' @rdname complog
##' @param x not used
comp_forget <- function(x) {
  db[["complog"]] %>% drop_model
  y <- db[["complog"]] %>% distinct(project) %>% select(project) %>% unlist
  o <- list.files(y,pattern="*\\.o$",full.names=TRUE)
  cppcpp <- list.files(y,pattern="*\\.cpp\\.cpp",full.names=TRUE)

  unlink(o)
  unlink(cppcpp)

  complog0 %>% comp_assign
  return(invisible(NULL))
}


## same_md5 <- function(x) tools::md5sum(cfile(x)) == grab_file(cfile(x))$md5

## skip_build <- function(x) {
##     a <- grab_file(cfile(x))
##     if(!same_md5(x)) return(FALSE)
##     if(!is.element(a$so,loaded_dll_files())) return(FALSE)
##     return(TRUE)
## }

compdate <- function(x) return(shdate(grab_file(x@model)$time))

complast <- function(x) assign("last",ntime(), envir=db)
get_complast <- function() return(db[["last"]])



db <- new.env()
db$complog <- complog0
db$last <- 0





