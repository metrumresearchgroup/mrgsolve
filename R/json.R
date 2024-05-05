# 
# l <- list()
# l$mrgsolve <- as.character(packageVersion("mrgsolve"))
# l$version <- 1
# l$model <- mod@model
# l$param <- as.list(param(mod))
# l$cmt <- as.list(init(mod))
# l$start <- mod$start
# l$end <- mod$end
# l$delta <- mod$delta
# l$add <- mod$add
# l$env <- as.list(mod@envir)
# l$atol <- mod@atol
# l$rtol <- mod@rtol
# l$ss_atol <- mod@ss_atol
# l$ss_rtol <- mod@ss_rtol
# l$maxsteps <- mod@maxsteps
# l$hmax <- mod@hmax
# l$hmin <- mod@hmin
# l$maxsteps <- mod@maxsteps
# l$mxhnil <- mod@mxhnil
# l$plugin <- mod@plugin
# l$capture <- mod@capture
# 
# l$omega <- lapply(as.list(omat(mod)), as.matrix)
# l$omega_labels <- labels(omat(mod))
# l$sigma <- lapply(as.list(mod@sigma), as.matrix)
# l$sigma_labels <- labels(smat(mod))
# 
# code <- gsub("\\t", "  ", mod@code, perl = TRUE)
# code <- modelparse(code)
# code <- lapply(code, trimws, which = "right")
# 
# clob <- c("PARAM", "INPUT", "THETA", "CMT", "INIT", "OMEGA", "SIGMA",
#           "NMEXT", "NMXML", "VCMT")
# for(block in clob) {
#   while(block %in% names(code)) {
#     code[[block]] <- NULL
#   }
# }
# 
# if("PKMODEL" %in% names(code)) {
#   pk <- mrgsolve:::tolist(code$PKMODEL)
#   e <- new.env()
#   pk$env <- e
#   pk <- do.call(mrgsolve:::PKMODEL, pk)
# }
# 
# code <- Map(code, names(code), f = function(text, name) {
#   c(glue("${name}"), text)
# })
# 
# l$code <- unlist(code)
# 
# code <- toJSON(l, pretty = TRUE, digits = NA)
# 
# writeLines(text = code, con = "foo.json")
# 
# x <- fromJSON(readLines("foo.json"))
# 
# 
# x
