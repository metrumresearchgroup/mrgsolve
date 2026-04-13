addin_run <- function(ctx, apply_fn) {
  sel <- ctx$selection[[1]]
  text <- sel$text
  if(!nchar(text)) { 
    # error and warning don't work quite the way I want
    msg <- c(x = "No text selected; please select the code to process.")
    inform(msg); 
    return(invisible(NULL))
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  result <- apply_fn(text, lines)
  # Don't each blank line at end of selection
  if(endsWith(text, "\n")) result <- paste0(result, "\n")
  rstudioapi::insertText(location = sel$range, text = result, id = ctx$id)
  invisible(NULL)
}

# Convert NM plugin -------------------------------------------------
apply_convert_nm <- function(text, lines) {
  has_block_markers <- function(x) any(grepl(block_re, x))
  if(has_block_markers(lines)) {
    result <- split_and_preprocess(text)
  } else {
    result <- convert_pow(lines)
    result <- convert_fort_if(result)
    result <- convert_semicolons(result)
    result <- add_leading_eq_space(result)
  }
  paste(result, collapse = "\n")
}

addin_preprocess <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  addin_run(ctx, apply_convert_nm)
}

# Add semicolons plugin ---------------------------------------------
apply_semicolons <- function(text, lines) {
  has_block_markers <- function(x) any(grepl(block_re, x))
  if(has_block_markers(lines)) {
    result <- split_and_convert_semicolons(text)
  } else {
    result <- convert_semicolons(lines)
  }
  paste(result, collapse = "\n")
}

addin_add_semicolons <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  addin_run(ctx, \(text, lines) apply_semicolons(text, lines))
}

# Convert pow ---------------------------------------------
apply_convert_pow <- function(text, lines) {
  has_block_markers <- function(x) any(grepl(block_re, x))
  if(has_block_markers(lines)) {
    result <- split_and_convert_pow(text)
  } else {
    result <- convert_pow(lines)
  }
  paste(result, collapse = "\n")
}

addin_convert_pow <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  addin_run(ctx, \(text, lines) apply_convert_pow(text, lines))
}
