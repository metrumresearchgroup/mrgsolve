addin_run <- function(ctx, apply_fn) {
  sel <- ctx$selection[[1]]
  text <- sel$text
  if(!nchar(text)) { 
    msg <- c(x = "No text selected; please select the code to process.")
    inform(msg); 
    return(invisible(NULL))
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  result <- apply_fn(text, lines)
  if(endsWith(text, "\n")) result <- paste0(result, "\n")
  rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  invisible(NULL)
}

addin_preprocess <- function() {
  has_block_markers <- function(x) any(grepl(block_re, x))
  apply_convert_nm <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_preprocess(text)
    } else {
      result <- add_operator_spaces(convert_semicolons(convert_fort_if(convert_pow(lines))))
    }
    paste(result, collapse = "\n")
  }
  addin_run(rstudioapi::getActiveDocumentContext(), apply_convert_nm)
}

addin_preprocess_condensed <- function() {
  has_block_markers <- function(x) any(grepl(block_re, x))
  apply_convert_nm <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_preprocess_condensed(text)
    } else {
      result <- strip_nonleading_spaces(convert_semicolons(convert_fort_if(convert_pow(lines))))
    }
    paste(result, collapse = "\n")
  }
  addin_run(rstudioapi::getActiveDocumentContext(), apply_convert_nm)
}

addin_preprocess_padded <- function() {
  has_block_markers <- function(x) any(grepl(block_re, x))
  apply_convert_nm <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_preprocess(text)
    } else {
      result <- add_operator_spaces(convert_semicolons(convert_fort_if(convert_pow(lines))))
    }
    paste(result, collapse = "\n")
  }
  addin_run(rstudioapi::getActiveDocumentContext(), apply_convert_nm)
}


addin_add_semicolons <- function() {
  has_block_markers <- function(x) any(grepl(block_re, x))
  apply_semicolons <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_add_semicolons(text)
    } else {
      result <- convert_semicolons(lines)
    }
    paste(result, collapse = "\n")
  }
  addin_run(rstudioapi::getActiveDocumentContext(), apply_semicolons)
}
