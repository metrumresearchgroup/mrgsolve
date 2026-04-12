addin_preprocess <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]
  text <- sel$text

  has_block_markers <- function(x) any(grepl(block_re, x))

  apply_convert_nm <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_preprocess(text)
    } else {
      result <- add_operator_spaces(convert_semicolons(convert_fort_if(convert_pow(lines))))
    }
    paste(result, collapse = "\n")
  }

  if(!nchar(text)) {
    lines <- ctx$contents
    result <- apply_convert_nm(paste(lines, collapse = "\n"), lines)
    n <- length(lines)
    full_range <- rstudioapi::document_range(
      rstudioapi::document_position(1, 0),
      rstudioapi::document_position(n, nchar(lines[[n]]))
    )
    rstudioapi::insertText(location = full_range, text = result, id = ctx$id)
  } else {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    result <- apply_convert_nm(text, lines)
    rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  }

  invisible(NULL)
}

addin_preprocess_condensed <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]
  text <- sel$text

  has_block_markers <- function(x) any(grepl(block_re, x))

  apply_convert_nm <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_preprocess_condensed(text)
    } else {
      result <- strip_nonleading_spaces(convert_semicolons(convert_fort_if(convert_pow(lines))))
    }
    paste(result, collapse = "\n")
  }

  if(!nchar(text)) {
    lines <- ctx$contents
    result <- apply_convert_nm(paste(lines, collapse = "\n"), lines)
    n <- length(lines)
    full_range <- rstudioapi::document_range(
      rstudioapi::document_position(1, 0),
      rstudioapi::document_position(n, nchar(lines[[n]]))
    )
    rstudioapi::insertText(location = full_range, text = result, id = ctx$id)
  } else {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    result <- apply_convert_nm(text, lines)
    rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  }

  invisible(NULL)
}

addin_preprocess_padded <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]
  text <- sel$text

  has_block_markers <- function(x) any(grepl(block_re, x))

  apply_convert_nm <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_preprocess(text)
    } else {
      result <- add_operator_spaces(convert_semicolons(convert_fort_if(convert_pow(lines))))
    }
    paste(result, collapse = "\n")
  }

  if(!nchar(text)) {
    lines <- ctx$contents
    result <- apply_convert_nm(paste(lines, collapse = "\n"), lines)
    n <- length(lines)
    full_range <- rstudioapi::document_range(
      rstudioapi::document_position(1, 0),
      rstudioapi::document_position(n, nchar(lines[[n]]))
    )
    rstudioapi::insertText(location = full_range, text = result, id = ctx$id)
  } else {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    result <- apply_convert_nm(text, lines)
    rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  }

  invisible(NULL)
}

addin_add_padding <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]
  text <- sel$text

  has_block_markers <- function(x) any(grepl(block_re, x))

  apply_padding <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_add_padding(text)
    } else {
      result <- add_operator_spaces(lines)
    }
    paste(result, collapse = "\n")
  }

  if(!nchar(text)) {
    lines <- ctx$contents
    result <- apply_padding(paste(lines, collapse = "\n"), lines)
    n <- length(lines)
    full_range <- rstudioapi::document_range(
      rstudioapi::document_position(1, 0),
      rstudioapi::document_position(n, nchar(lines[[n]]))
    )
    rstudioapi::insertText(location = full_range, text = result, id = ctx$id)
  } else {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    result <- apply_padding(text, lines)
    rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  }

  invisible(NULL)
}

addin_add_semicolons <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]
  text <- sel$text
  
  has_block_markers <- function(x) any(grepl(block_re, x))

  apply_semicolons <- function(text, lines) {
    if(has_block_markers(lines)) {
      result <- split_and_add_semicolons(text)
    } else {
      result <- convert_semicolons(lines)
    }
    paste(result, collapse = "\n")
  }

  if(!nchar(text)) {
    lines <- ctx$contents
    result <- apply_semicolons(paste(lines, collapse = "\n"), lines)
    n <- length(lines)
    full_range <- rstudioapi::document_range(
      rstudioapi::document_position(1, 0),
      rstudioapi::document_position(n, nchar(lines[[n]]))
    )
    rstudioapi::insertText(location = full_range, text = result, id = ctx$id)
  } else {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    result <- apply_semicolons(text, lines)
    rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  }

  invisible(NULL)
}
