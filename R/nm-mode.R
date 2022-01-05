new_nm_obj <- function() {
  data <- data.frame(match = 0, prefix = 0, cmt = 0)[0,]
  list(
    found_any = FALSE, 
    found_frda = FALSE, 
    has_ode = FALSE,
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
  stopifnot(is.list(spec))
  ans <- new_nm_obj()
  ans[["has_ode"]] <- "ODE" %in% names(spec)
  FRDA <- c("F", "R", "D", "ALAG")
  # CHeck non-ODE
  blocks_to_check <- c("PREAMBLE", "MAIN", "TABLE")
  pmt <- unlist(spec[blocks_to_check], use.names = FALSE)
  m1 <- find_nm_vars_impl(pmt)
  # Check ODE
  m2 <- find_nm_vars_impl(spec[["ODE"]])
  m <- rbind(m1, m2)
  if(ans[["found_any"]] <- nrow(m) > 0) {
    names(m) <- names(ans[["match"]])
    m <- m[!duplicated(m[["match"]]),]
    m[["cmt"]] <- as.numeric(m[["cmt"]])
    is_frda <- as.integer(m[["prefix"]] %in% FRDA)
    m <- m[order(is_frda, m[["prefix"]], m[["cmt"]]),, drop = FALSE]
    ans[["frda"]] <- m[m[["prefix"]] %in% FRDA,,drop=FALSE]    
    rownames(m) <- NULL
    ans[["match"]] <- m
    ans[["cmtn"]] <- sort(unique(m[["cmt"]]))
    if(nrow(m2) > 0) {
      names(m2) <- names(ans[["match"]])
      ans[["ddt"]] <- m2[m2[["prefix"]] == "DADT",,drop = FALSE]
      ans[["ddt"]][["cmt"]] <- as.numeric(ans[["ddt"]][["cmt"]])
      ans[["dcmtn"]] <- sort(unique(ans[["ddt"]][["cmt"]]))
    }
  } 
  return(ans)
}

find_nm_vars_impl <- function(code) {
  nul <- data.frame(V1 = 0, V2 = 0, V3 = 0)[0,]
  if(!is.character(code)) return(nul)
  re1 <- "\\b(A|A_0|DADT)\\(([0-9]+)\\)"
  re2 <- "\\b(F|R|D|ALAG)([0-9]+)\\b"
  # gregexec exists in R 4.1; rolling my own for now
  m1 <- gregexecdf(re1, code) 
  m2 <- gregexecdf(re2, code) 
  m <- rbind(m1, m2)
  if(nrow(m)==0) {
    return(nul)  
  }
  m
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
  ans
}

any_nm_vars <- function(x) {
  m1 <- regmatches(x, gregexpr("\\b(F|R|D|ALAG)[0-9]+\\b", x))
  m2 <- x[x %in% Reserved_nm]
  ans <- unlist(c(m1, m2), use.names=FALSE)
  list(found_any = length(ans) > 0, match = ans)
}

audit_nm_vars <- function(x, param, init, build, nmv, env) {
  bad_param <- any_nm_vars(names(param))
  bad_init <- any_nm_vars(names(init))
  bad_cpp <- any_nm_vars(build[["cpp_variables"]][["var"]])
  audit_dadt <- isTRUE(env[["audit_dadt"]]) && length(nmv[["dcmtn"]]) > 0
  err <- c()
  if(bad_param[["found_any"]]) {
    err <- c(err, "Reserved names in parameter list:")
    msg <- paste0("--| reserved: ", bad_param[["match"]])
    err <- c(err, msg)
  }
  if(bad_init[["found_any"]]) {
    err <- c(err, "Reserved names in compartment list:")
    msg <- paste0("--| reserved: ", bad_init[["match"]])
    err <- c(err, msg)
  }
  if(bad_cpp[["found_any"]]) {
    err <- c(err, "Reserved names in cpp variable list:")
    msg <- paste0("--| reserved: ", bad_cpp[["match"]])
    err <- c(err, msg)
  }
  cmtn <- seq_along(init)
  if(length(cmtn) > 0) {
    err <- c(err, audit_nm_vars_range(nmv, cmtn, audit_dadt = audit_dadt))
  }
  if(length(err) > 0) {
    msg <- "improper use of special variables with [nm-vars] plugin\n"
    err <- paste0(c(msg, err), collapse = "\n")
    stop(err, call. = FALSE)  
  }
  return(invisible(TRUE))
}

autodec_nm_vars <- function(x, env) {
  if(!env[["using_nm-vars"]]) return(invisible(TRUE))
  err <- c()
  if(any(x %in% Reserved_nm)) {
    bad <- intersect(x, Reserved_nm)
    err <- c(err, "Reserved names found via autodec:")
    err <- c(err, paste0("--| reserved: ", bad))
  }
  if(length(err) > 0) {
    msg <- "improper use of special variables with [nm-vars] plugin\n"
    err <- paste0(c(msg, err), collapse = "\n")
    stop(err, call. = FALSE)  
  }
  return(invisible(TRUE))
}

audit_nm_vars_range <- function(x, cmtn, audit_dadt) {
  err <- c()
  # Look for compartment indices out of range
  m <- x[["match"]]
  if(!all(m[["cmt"]] %in% cmtn)) {
    bad <- m[!(m[["cmt"]] %in% cmtn),,drop = FALSE]
    valid <- paste0(range(cmtn), collapse = " to ")
    valid <- paste0("Valid compartment range: ", valid)
    err <- c(err, valid)
    for(b in seq(nrow(bad))) {
      err <- c(err, paste0("--| out of range: ", bad[b, "match"]))
    }
  }
  # Make sure there are ODEs for every compartment
  if(x[["has_ode"]] && isTRUE(audit_dadt)) {
    bad <- setdiff(cmtn, x[["ddt"]][["cmt"]])
    if(length(bad) > 0) {
      err <- c(err, "Missing differential equation(s):")
      for(b in bad) {
        err <- c(err, paste0("--| missing: DADT(", b, ")"))   
      }
      err <- c(err, paste0("--| suppress with @!audit block option"))
    }
  }
  return(err)
}
