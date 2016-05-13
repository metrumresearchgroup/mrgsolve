make_worksheet <- function(file) {
  stem <- gsub("\\.[Rr]$", "", file)
  ofile <- paste0(stem, "_worksheet.R")
  x <- readLines(file)
  keep <- grep("^\\s*(\\#\\#\\'|\\#\\+)",x)
  keep_code <- grep("\\#\\s*$",x)
  erase <- setdiff(1:length(x),c(keep,keep_code))
  x[erase] <- ""
  x <- paste(x,collapse="\n")
  x <- gsub("\\n\\n\\n+", "\n\n",x)
  x <- strsplit(x, "\n")[[1]]          
  cat(file="worksheet.R", x,sep="\n")
}

