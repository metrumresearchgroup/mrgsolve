new_nm_obj <- function() {
  data <- data.frame(match = 0, prefix = 0, cmt = 0)[0,]
  list(
    found_any = FALSE, 
    found_frda = FALSE, 
    has_ode = FALSE,
    reserved = character(0),
    match = data, 
    frda = data, 
    ddt = data, 
    cmtn = integer(0), 
    dcmtn = integer(0)
  )
}

#' Scan model code for nonmem-style variables
#' 
#' @keywords internal
#' @noRd
find_nm_vars <- function(spec) {
  FRDA <- c("F", "R", "D", "ALAG")
  PREFIX <- c("A", "A_0", "DADT")
  blocks_to_check <- c("PREAMBLE", "MAIN", "TABLE")
  pmt <- unlist(spec[blocks_to_check], use.names = FALSE)
  m1 <- find_nm_vars_impl(pmt)
  m2 <- find_nm_vars_impl(spec[["ODE"]])
  m <- rbind(m1, m2)
  ans <- new_nm_obj()
  ans[["has_ode"]] <- "ODE" %in% names(spec)
  if(ans[["found_any"]] <- nrow(m) > 0) {
    names(m) <- names(ans[["match"]])
    m <- m[!duplicated(m[["match"]]),]
    m[["type"]] <- as.integer(m[["prefix"]] %in% FRDA)
    m[["cmt"]] <- as.numeric(m[["cmt"]])
    m <- m[order(m[["type"]], m[["prefix"]], m[["cmt"]]),, drop = FALSE]
    ans[["match"]] <- m
    ans[["cmtn"]] <- sort(unique(m[["cmt"]]))
    if(nrow(m2) > 0) {
      names(m2) <- names(ans[["match"]])
      ans[["ddt"]] <- filter(m2, .data[["prefix"]] == "DADT")
      ans[["dcmtn"]] <- sort(unique(ans[["ddt"]][["cmt"]]))
    }
    ans[["frda"]] <- filter(m, .data[["prefix"]] %in% FRDA)
  } 
  return(ans)
}

find_nm_vars_impl <- function(code) {
  if(!is.character(code)) return(data.frame())
  re1 <- "(A|A_0|DADT)\\(([0-9]+)\\)"
  re2 <- "\\b(F|R|D|ALAG)([0-9]+)\\b"
  m1 <- regmatches(code, gregexec(re1, code))
  m2 <- regmatches(code, gregexec(re2, code))
  m <- do.call(cbind, c(m1, m2))
  if(ncol(m)==0) {
    return(data.frame(V1 = 0, V2 = 0, V3 = 0)[0,])  
  }
  as.data.frame(t(m), stringsAsFactors = FALSE)
}

generate_nmdefs <- function(x) {
  if(isFALSE(x[["found_any"]])) return(NULL)
  ans <- paste0(
    "#define ", 
    x[["frda"]][["match"]],
    " ", 
    "_", 
    x[["frda"]][["prefix"]], 
    "_[", 
    x[["frda"]][["cmt"]] - 1, 
    "]"
  )
  ans <- c(
    "#define A(a) _A_[a-1]", 
    "#define A_0(a) _A_0_[a-1]",
    "#define DADT(a) _DADT_[a-1]",
    ans  
  )
  ans
}

any_frda <- function(x) {
  m <- regmatches(x, gregexpr("\\b(F|R|D|ALAG)[0-9]+\\b", x))  
  ans <- unlist(m, use.names=FALSE)
  list(found_any = length(ans) > 0, match = ans)
}

audit_nm_vars <- function(x, param, init, build, nmv) {
  bad_param <- any_frda(names(param))
  bad_init <- any_frda(names(init))
  bad_cpp <- any_frda(build[["cpp_variables"]][["var"]])
  err <- c()
  if(bad_param[["found_any"]]) {
    err <- c(err, "Reserved names in parameter list:")
    msg <- paste0(" [nm-vars] reserved: ", bad_param[["match"]])
    err <- c(err, msg)
  }
  if(bad_init[["found_any"]]) {
    err <- c(err, "Reserved names in compartment list:")
    msg <- paste0(" [nm-vars] reserved: ", bad_init[["match"]])
    err <- c(err, msg)
  }
  if(bad_cpp[["found_any"]]) {
    err <- c(err, "Reserved names in cpp variable list:")
    msg <- paste0(" [nm-vars] reserved: ", bad_cpp[["match"]])
    err <- c(err, msg)
  }
  cmtn <- seq_along(init)
  if(length(cmtn) > 0) {
    err <- c(err, audit_nm_vars_range(nmv, cmtn))
  }
  if(length(err) > 0) {
    tmp <- sapply(err, message)
    stop(
      "improper use of special variables with nm-vars plugin", 
      call. = FALSE
    )  
  }
  return(invisible(TRUE))
}

audit_nm_vars_range <- function(x, cmtn) {
  err <- c()
  # Look for compartment indices out of range
  m <- x[["match"]]
  if(!all(m[["cmt"]] %in% cmtn)) {
    bad <- filter(m, !(.data[["cmt"]] %in% cmtn))
    valid <- paste0(range(cmtn), collapse = " to ")
    valid <- paste0("Valid compartment range: ", valid)
    err <- c(err, valid)
    for(b in seq(nrow(bad))) {
      err <- c(err, paste0(" [nm-vars] out of range: ", bad[b, "match"]))
    }
  }
  # Make sure there are ODEs for every compartment
  if(x[["has_ode"]]) {
    bad <- setdiff(cmtn, x[["ddt"]][["cmt"]])
    if(length(bad) > 0) {
      err <- c(err, "Missing differential equation(s):")
      for(b in bad) {
        err <- c(err, paste0(" [nm-vars] missing: DADT(", b, ")"))   
      }
    }
  }
  return(err)
}
