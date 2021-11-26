
#' Scan model code for nonmem-style variables
#' 
#' @keywords internal
#' @noRd
audit_nm_vars <- function(code) {
  re1 <- "(DADT|A|A_0)\\(([0-9]+)\\)"
  re2 <- "\\b(F|ALAG|D|R)([0-9]+)\\b"
  m1 <- regmatches(code, gregexec(re1, code, perl = TRUE))
  m2 <- regmatches(code, gregexec(re2, code, perl = TRUE))
  m <- do.call(cbind,c(m1, m2))
  if(found_any <- nrow(m) > 0) {
    ddt <- m[,which(m[2,]=="DADT")]
    range <- sort(unique(as.numeric(m[3, ])))
    ddt_range <- sort(unique(as.numeric(ddt[3, ])))
  } else {
    ddt <- range <- ddt_range <- integer(0)
  }
  list(
    found_any = found_any,
    match = m, 
    ddt = ddt, 
    range = range, 
    ddt_range = ddt_range
  )
}

nm_pp_defs <- function(cmt) {
  i <- seq_along(cmt)
  c( 
    paste0("#define DADT(", i, ")  _DADT_[", i-1, "]"), 
    paste0("#define A(", i,    ")     _A_[", i-1, "]"),
    paste0("#define A_0(", i,  ")   _A_0_[", i-1, "]"),
    paste0("#define F", i,     "      _F_[", i-1, "]"),
    paste0("#define D", i,     "      _D_[", i-1, "]"),
    paste0("#define R", i,     "      _R_[", i-1, "]"),
    paste0("#define ALAG", i,  "   _ALAG_[", i-1, "]")
  )
}


