addin_run <- function(ctx, apply_fn) {
  sel <- ctx$selection[[1]]
  text <- sel$text
  if(!nchar(text)) {
    lines <- ctx$contents
    result <- apply_fn(paste(lines, collapse = "\n"), lines)
    n <- length(lines)
    full_range <- rstudioapi::document_range(
      rstudioapi::document_position(1, 0),
      rstudioapi::document_position(n, nchar(lines[[n]]))
    )
    rstudioapi::insertText(location = full_range, text = result, id = ctx$id)
  } else {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    result <- apply_fn(text, lines)
    if(endsWith(text, "\n")) result <- paste0(result, "\n")
    rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  }
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

addin_add_padding <- function() {
  has_block_markers <- function(x) any(grepl(block_re, x))
  apply_padding <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_add_padding(text)
    } else {
      result <- add_operator_spaces(lines)
    }
    paste(result, collapse = "\n")
  }
  addin_run(rstudioapi::getActiveDocumentContext(), apply_padding)
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
